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

class HsLooper {
public:
    HsLooper();
    ~HsLooper();
    
    void process_samples(uint32 window_size, uint32 in_channels, uint32 out_channels, float** channel_data);
    std::string uilog();
private:
    void* _state;
};

#endif /* HsLooper_h */
