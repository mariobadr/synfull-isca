/*
 * fes2_interface.hpp
 *
 *  Created on: Apr 10, 2010
 *      Author: robert
 */

#ifndef FES2_INTERFACE_HPP_
#define FES2_INTERFACE_HPP_

//#define FES2_INTERFACE_ACTIVE

#include "module.hpp"
#include "config_utils.hpp"
#include "network.hpp"
#include "socketstream.h"
#include "messages.h"
#include "trace_generator.hpp"

#include <queue>
#include <map>


struct FeS2RequestPacket {
	int source;
	int dest;
	int id;
	int size;
	int network;
	int cl;
	int miss_pred;
};

struct FeS2ReplyPacket {
	int source;
	int dest;
	int id;
	int network;
	int cl;
	int miss_pred;
};


class FeS2Interface {

private:
	queue<FeS2RequestPacket *> **_request_buffer;
	queue<FeS2ReplyPacket *> _reply_buffer;

	map<int, int> _original_destinations;

	TraceGenerator *_trace;

	int _trace_mode;

	int _sources;
	int _dests;

	int _duplicate_networks;

	bool _concentrate;

	SocketStream _listenSocket;
	SocketStream *_channel;
	string _host;
	int _port;

	map<int, int> _node_map;

public:
	FeS2Interface( const Configuration &config, const vector<BSNetwork *> & net );
	~FeS2Interface();

	int Init();
	int Step();

	int EnqueueFeS2RequestPacket(FeS2RequestPacket *packet);
	FeS2RequestPacket *DequeueFeS2RequestPacket(int source, int network, int cl);

	int getFeS2ReplyQueueSize() { return _reply_buffer.size(); }
	int EnqueueFeS2ReplyPacket(FeS2ReplyPacket *packet);
	FeS2ReplyPacket *DequeueFeS2ReplyPacket();

	int GenerateTestPackets();


};

#endif /* FES2_INTERFACE_HPP_ */
