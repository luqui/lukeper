//
//  HsLooper.cpp
//  lukeper
//
//  Created by Luke Palmer on 3/28/17.
//
//

#include "HsLooper.h"

extern "C" {
    void hs_init(int*, char***);
    void hs_add_root(void (*)());
    void hs_exit();
    
    void __stginit_Looper();
    void* hs_looper_init();
    void hs_looper_main(void* state, int window, int in_channels, int out_channels, float** channel_data);
    void hs_looper_exit(void* state);
}

HsLooper::HsLooper() {
    hs_init(0, 0);
    hs_add_root(__stginit_Looper);
    
    _state = hs_looper_init();
}

HsLooper::~HsLooper() {
    hs_looper_exit(_state);
    hs_exit();
}

void HsLooper::process_samples(int window_size, int in_channels, int out_channels, float** channel_data) {
    hs_looper_main(_state, window_size, in_channels, out_channels, channel_data);
}
