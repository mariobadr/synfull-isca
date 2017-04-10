/*
 * FeS2Interface1.cpp
 *
 *  Created on: Apr 10, 2010
 *      Author: robert
 */

#include <sstream>

#include "fes2_interface.hpp"

FeS2Interface::FeS2Interface( const Configuration &config,
		const vector<BSNetwork *> & net) {

	_trace_mode = config.GetInt("trace_mode");

	if (_trace_mode == 1) {
		_trace = new TraceGenerator();

		string trace_file_name;
		trace_file_name = config.GetStr("trace_file");

		if (!_trace->openTraceFile( trace_file_name )) {
			cerr << "Trace file cannot be opened: " << trace_file_name << endl;
		}
	}

	_channel = NULL;

	_sources = net[0]->NumNodes( );
	_dests   = net[0]->NumNodes( );

	_duplicate_networks = config.GetInt("subnets");

	_concentrate = config.GetInt("fes2_concentrate") ? true : false;

	_host = config.GetStr( "fes2_host");
	_port = config.GetInt("fes2_port");

	vector<int> mapping = config.GetIntArray("fes2_mapping");
	for(int i = 0; i < mapping.size(); i++) {
		_node_map[i] = mapping[i];
	}

	_request_buffer = new queue<FeS2RequestPacket *> * [_duplicate_networks];
	for (int i=0; i < _duplicate_networks; i++ ) {
		_request_buffer[i] = new queue<FeS2RequestPacket *> [_sources];
	}

}

FeS2Interface::~FeS2Interface() {
	if (_trace_mode == 1) {
		_trace->closeTraceFile();
	}

	for ( int n = 0; n < _duplicate_networks; ++n ) {
		delete [] _request_buffer[n];
	}
	delete [] _request_buffer;
}

int FeS2Interface::Init() {
	// Start listening for incoming connections

	// TODO Configurable host/port
	// if (_listenSocket.listen(_host.c_str(), _port) < 0)

	// constants available in both BookSim and FeS2
	if (_listenSocket.listen(NS_HOST, NS_PORT) < 0) {
		return -1;
	}

	// Wait for FeS2 to connect
	_channel = _listenSocket.accept();
#ifdef NS_DEBUG
	cout << "FeS2 instance connected" << endl;
#endif
	// Initialize client
	InitializeReqMsg req;
	InitializeResMsg res;
	*_channel >> req << res;

	return 0;
}

int FeS2Interface::Step() {
	bool process_more = true;
	StreamMessage *msg = NULL;

	while (process_more && _channel && _channel->isAlive())
	{
		// read message
		*_channel >> (StreamMessage*&) msg;


		switch(msg->type)
		{
		case STEP_REQ:
		{
			// acknowledge the receipt of step request
			// we're actually doing a little bit of work in parallel
			StepResMsg res;
			*_channel << res;

			// fall-through and perform one step of BookSim loop
			process_more = false;

			break;
		}
		case INJECT_REQ:
		{
			InjectReqMsg* req = (InjectReqMsg*) msg;

			// create packet to store in local queue
			FeS2RequestPacket* rp = new FeS2RequestPacket();
			rp->cl = req->cl;
			rp->dest = req->dest;
			rp->id = req->id;
			rp->network = req->network;
			rp->size = req->packetSize;
			rp->source = req->source;
			rp->miss_pred = req->miss_pred;


			if (_trace_mode == 1) {
				//_trace->writeTraceItem(GetSimTime(), rp->source, rp->dest,
				//		rp->size, req->address, rp->network);
				stringstream str;

				str << rp->id << " " << GetSimTime() << " " << rp->source << " "
						<< rp->dest << " " << rp->size << " " << req->msgType
						<< " " << req->coType << " " << req->address;

				_trace->writeTrace(str.str());
			}

			EnqueueFeS2RequestPacket(rp);

			// acknowledge receipt of packet to FeS2
			InjectResMsg res;
			*_channel << res;

			break;
		}
		case EJECT_REQ:
		{
			EjectReqMsg* req = (EjectReqMsg*) msg;
			EjectResMsg res;
			// create packet to store in local queue
			FeS2ReplyPacket* rp = DequeueFeS2ReplyPacket();

			if (rp != NULL)
			{
				res.source = rp->source;
				res.dest = rp->dest;
				res.network = rp->network;
				res.id = rp->id;
				res.cl = rp->cl;
				res.miss_pred = rp->miss_pred;

				// free up reply packet
				free(rp);
				rp = NULL;
			}
			else
			{
				res.id = -1;
			}

			res.remainingRequests = getFeS2ReplyQueueSize();

			*_channel << res;

			break;
		}
		case QUIT_REQ:
		{
			// acknowledge quit
			QuitResMsg res;
			*_channel << res;

			return 1; // signal that we're done

			break;
		}
		default:
		{
			cout << "<FeS2Interface::Step> Unknown message type: "
					<< msg->type << endl;
			break;
		}
		} // end switch

		// done processing message, destroy it
		StreamMessage::destroy(msg);
	}

	return 0;
}


int FeS2Interface::EnqueueFeS2RequestPacket(FeS2RequestPacket *packet) {
	//The mapping of FeS2 devices to BookSim nodes is done here

	// We always need to store the original destination send it to the correct
	// FeS2 queue
	_original_destinations[packet->id] = packet->dest;

	//_node_map is based off of the configuration file. See "fes2_mapping"
	packet->source 	= _node_map[packet->source];
	packet->dest 	= _node_map[packet->dest];

	// special case: single network
	if (_duplicate_networks == 1) {
		_request_buffer[0][packet->source].push(packet);
	} else {
		assert (packet->network < _duplicate_networks);
		_request_buffer[packet->network][packet->source].push(packet);
	}
	return 0;
}

FeS2RequestPacket *FeS2Interface::DequeueFeS2RequestPacket(int source,
		int network, int cl) {
	FeS2RequestPacket *packet = NULL;

	if (!_request_buffer[network][source].empty()) {
		packet = _request_buffer[network][source].front();
		if (packet->cl == cl) {
			_request_buffer[network][source].pop();
		} else {
			packet = 0;
		}
	}

	return packet;
}

int FeS2Interface::EnqueueFeS2ReplyPacket(FeS2ReplyPacket *packet) {
	assert(_original_destinations.find(packet->id) !=
			_original_destinations.end());

	if (_concentrate) {
		assert(_original_destinations.find(packet->id) !=
				_original_destinations.end());
		assert(_original_destinations[packet->id]/2 == packet->dest);
		packet->source *= 2;
	}

	packet->dest = _original_destinations[packet->id];
	_original_destinations.erase(packet->id);

	_reply_buffer.push(packet);

	return 0;
}

FeS2ReplyPacket *FeS2Interface::DequeueFeS2ReplyPacket() {
	FeS2ReplyPacket *packet = NULL;

	if (!_reply_buffer.empty()) {
		packet = _reply_buffer.front();
		_reply_buffer.pop();
	}

	return packet;
}

int FeS2Interface::GenerateTestPackets() {
	return 0;
}
