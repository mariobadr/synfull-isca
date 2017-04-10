/* 
Copyright (c) 2014, Mario Badr
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "NetworkInterface.h"
#include <sstream>

/**
 * Establish this as a server over a socket and wait for the client (TrafficManager)
 * to connect.
 */
int NetworkInterface::Init() {
	SocketStream listenSocket;

	if(listenSocket.listen() < 0) {
		return -1;
	}

	m_channel = listenSocket.accept();

	// Initialize client
	InitializeReqMsg req;
	InitializeResMsg res;
	*m_channel >> req << res;

	return 0;
}

/**
 * Called by your network simulator. This will ask the TrafficGenerator to: produce
 * traffic and send it over the socket, accept packets that have arrived at their
 * destination (based on your network simulator), indicate that all packets have
 * been created for the current cycle (STEP_REQ), or end the simulation.
 *
 * @return 0 if the simulation is to continue, 1 if the simulation is to end.
 */
int NetworkInterface::Step() {
	bool process_more = true;
	StreamMessage *msg = NULL;

	while (process_more && m_channel && m_channel->isAlive())
	{
		// read message
		*m_channel >> (StreamMessage*&) msg;
		switch(msg->type)
		{
		case STEP_REQ:
		{
			// acknowledge the receipt of step request
			StepResMsg res;
			*m_channel << res;

			// fall-through and increment your network one cycle
			process_more = false;

			break;
		}
		case INJECT_REQ:
		{
			//Cast the message into the packet to be injected
			InjectReqMsg* req = (InjectReqMsg*) msg;

			//Inject the packet into your network simulator here.

			// acknowledge receipt of packet to TrafficManager
			InjectResMsg res;
			*m_channel << res;

			break;
		}
		case EJECT_REQ:
		{
			EjectReqMsg* req = (EjectReqMsg*) msg;
			EjectResMsg res;
			
			//Take packets that have arrived at their destination (according to your
			//network simulator) and send them back by populating res with response 
			//information. Don't forget to set res.remainingRequests to a number 
			//greater than 0 until no more packets remain for this cycle.

			*m_channel << res;

			break;
		}
		case QUIT_REQ:
		{
			// acknowledge quit
			QuitResMsg res;
			*m_channel << res;

			return 1; // signal that we're done
		}
		default:
		{
			cout << "Unknown message type: " << msg->type << endl;
			break;
		}
		} // end switch

		// done processing message, destroy it
		StreamMessage::destroy(msg);
	}

	return 0;
}
