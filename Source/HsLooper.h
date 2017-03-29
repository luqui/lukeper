//
//  HsLooper.h
//  lukeper
//
//  Created by Luke Palmer on 3/28/17.
//
//

#ifndef HsLooper_h
#define HsLooper_h

#include "../JuceLibraryCode/JuceHeader.h"

struct MarshalMidiMessage {
    MarshalMidiMessage() { }
    MarshalMidiMessage(uint32 samplePosition, uint8 msg1, uint8 msg2, uint8 msg3) 
        : samplePosition(samplePosition)
    {
        message[0] = msg1;
        message[1] = msg2;
        message[2] = msg3;
    }

    uint32 samplePosition;
    uint8 message[3];
};

class HsLooper {
public:
    HsLooper();
    ~HsLooper();
    
    MarshalMidiMessage* process_samples(uint32 window_size, uint32 in_channels, uint32 out_channels, float** channel_data,
                                        uint32 midi_messages, MarshalMidiMessage* message_data, uint32* out_message_count);
private:
    void* _state;
};

#endif /* HsLooper_h */
