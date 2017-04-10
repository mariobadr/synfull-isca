/* 
Copyright (c) 2014, Mario Badr
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/*
 * messages.h
 *
 *  Created on: Dec 13, 2009
 *      Author: sam
 */

#ifndef MESSAGES_H_
#define MESSAGES_H_

#include "socketstream.h"
#include <stdint.h>

#define INVALID_MESSAGE -1
#define INITIALIZE_REQ  0
#define INITIALIZE_RES  1
#define STEP_REQ  		2
#define STEP_RES  		3
#define INJECT_REQ  	4
#define INJECT_RES  	5
#define EJECT_REQ  		6
#define EJECT_RES  		7
#define QUIT_REQ  		8
#define QUIT_RES  		9

struct StreamMessage
{
    int size;
    int type;
    StreamMessage() :
        size(-1), type(INVALID_MESSAGE)
    {
    }

    /*
     * Message sending function.
     */
    friend SocketStream& operator<<(SocketStream& os, StreamMessage& msg);

    /*
     * Message receiving function. Use this function if you don't know the message type in advance
     * NOTE: must destroy msg using Message::destroy(Message*)
     */
    friend SocketStream& operator>>(SocketStream& is, StreamMessage*& msg);

    /*
     * Message receiving function. Use this function if you know the message type in advance
     */
    friend SocketStream& operator>>(SocketStream& is, StreamMessage& msg);

    static void destroy(StreamMessage* msg);
};

struct InitializeReqMsg: StreamMessage
{
    InitializeReqMsg()
    {
        size = sizeof(InitializeReqMsg);
        type = INITIALIZE_REQ;
    }
};

struct InitializeResMsg: StreamMessage
{
    InitializeResMsg()
    {
        size = sizeof(InitializeResMsg);
        type = INITIALIZE_RES;
    }
};

struct StepReqMsg: StreamMessage
{
	StepReqMsg()
    {
        size = sizeof(StepReqMsg);
        type = STEP_REQ;
    }
};

struct StepResMsg: StreamMessage
{
	StepResMsg()
    {
        size = sizeof(StepResMsg);
        type = STEP_RES;
    }
};

struct InjectReqMsg: StreamMessage
{
	InjectReqMsg()
    {
        size = sizeof(InjectReqMsg);
        type = INJECT_REQ;
    }
	int source;
	int dest;
	int id;
	int packetSize;
	int network;
	int cl;
	int miss_pred;
	int msgType;
	int coType;
	unsigned long long address;
};

struct InjectResMsg: StreamMessage
{
	InjectResMsg()
    {
        size = sizeof(InjectResMsg);
        type = INJECT_RES;
    }
};

struct EjectReqMsg: StreamMessage
{
	EjectReqMsg()
    {
        size = sizeof(EjectReqMsg);
        type = EJECT_REQ;
    }
};

struct EjectResMsg: StreamMessage
{
	EjectResMsg()
    {
        size = sizeof(EjectResMsg);
        type = EJECT_RES;
    }
	int id;
	int remainingRequests;
	int source;
	int dest;
	int packetSize;
	int network;
	int cl;
	int miss_pred;

};

struct QuitReqMsg: StreamMessage
{
	QuitReqMsg()
    {
        size = sizeof(QuitReqMsg);
        type = QUIT_REQ;
    }
};

struct QuitResMsg: StreamMessage
{
	QuitResMsg()
    {
        size = sizeof(QuitResMsg);
        type = QUIT_RES;
    }
};

#endif /* MESSAGES_H_ */
