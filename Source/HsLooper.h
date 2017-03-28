//
//  HsLooper.h
//  lukeper
//
//  Created by Luke Palmer on 3/28/17.
//
//

#ifndef HsLooper_h
#define HsLooper_h

class HsLooper {
public:
    HsLooper();
    ~HsLooper();
    
    void process_samples(int window_size, int in_channels, int out_channels, float** channel_data);
private:
    void* _state;
};

#endif /* HsLooper_h */
