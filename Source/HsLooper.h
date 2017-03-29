//
//  HsLooper.h
//  lukeper
//
//  Created by Luke Palmer on 3/28/17.
//
//

#ifndef HsLooper_h
#define HsLooper_h

struct MarshalMidiMessage {
    MarshalMidiMessage() { }
    MarshalMidiMessage(int samplePosition, unsigned char msg1, unsigned char msg2, unsigned char msg3) 
        : samplePosition(samplePosition)
    {
        message[0] = msg1;
        message[1] = msg2;
        message[2] = msg3;
    }

    int samplePosition;
    unsigned char message[3];
};

class HsLooper {
public:
    HsLooper();
    ~HsLooper();
    
    MarshalMidiMessage* process_samples(int window_size, int in_channels, int out_channels, float** channel_data,
                                        int midi_messages, MarshalMidiMessage* message_data, int* out_message_count);
private:
    void* _state;
};

#endif /* HsLooper_h */
