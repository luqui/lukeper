//
//  HsLooper.cpp
//  lukeper
//
//  Created by Luke Palmer on 3/28/17.
//
//

#include "Rts.h"
#include "HsLooper.h"

extern "C" {
    void __stginit_Looper();
    void* hs_looper_init();
    void hs_looper_main(
        void* state,
        uint32 window,
        uint32 in_channels,
        uint32 out_channels,
        float** channel_data);
    char* hs_looper_uilog(void* state);
    void hs_looper_exit(void* state);
}

static bool hs_initialized = false;

HsLooper::HsLooper() {
    if (!hs_initialized) {
        RtsConfig conf = defaultRtsConfig;
        conf.rts_opts_enabled = RtsOptsAll;

        int argc = 4;
        char* argv[] = { "main", "+RTS", "-H134217728", "-RTS" };
        //int argc = 1;
        //char* argv[] = { "main" };
        char** argv_ = argv;

        hs_init_ghc(&argc, &argv_, conf);

        hs_add_root(__stginit_Looper);
        hs_initialized = true;
    }
    
    _state = hs_looper_init();
}

HsLooper::~HsLooper() {
    hs_looper_exit(_state);
    //hs_exit();
}

void HsLooper::process_samples(uint32 window_size, uint32 in_channels, uint32 out_channels, float** channel_data) {
    hs_looper_main(_state, window_size, in_channels, out_channels, channel_data);
}

std::string HsLooper::uilog() {
    char* ret = hs_looper_uilog(_state);
    std::string ret2(ret);
    free(ret);
    return ret2;
}
