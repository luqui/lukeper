/*
  ==============================================================================

    This file was auto-generated!

    It contains the basic framework code for a JUCE plugin processor.

  ==============================================================================
*/

#include "PluginProcessor.h"
#include "PluginEditor.h"

extern "C" {
    void __stginit_Looper();
    void hs_init(int*, char***);
    void hs_add_root(void (*)());
    void hs_exit();
}


//==============================================================================
LukeperAudioProcessor::LukeperAudioProcessor()
#ifndef JucePlugin_PreferredChannelConfigurations
     : AudioProcessor (BusesProperties()
                     #if ! JucePlugin_IsMidiEffect
                      #if ! JucePlugin_IsSynth
                       .withInput  ("Input",  AudioChannelSet::stereo(), true)
                      #endif
                       .withOutput ("Output", AudioChannelSet::stereo(), true)
                     #endif
                       )
     , _loopBuffer(44100)
     , _loopBufIndex(0)
     , _recording(true)
#endif
{
    hs_init(NULL, NULL);
    hs_add_root(__stginit_Looper);
    
}

LukeperAudioProcessor::~LukeperAudioProcessor()
{
    hs_exit();
}

//==============================================================================
const String LukeperAudioProcessor::getName() const
{
    return JucePlugin_Name;
}

bool LukeperAudioProcessor::acceptsMidi() const
{
   #if JucePlugin_WantsMidiInput
    return true;
   #else
    return false;
   #endif
}

bool LukeperAudioProcessor::producesMidi() const
{
   #if JucePlugin_ProducesMidiOutput
    return true;
   #else
    return false;
   #endif
}

double LukeperAudioProcessor::getTailLengthSeconds() const
{
    return 0.0;
}

int LukeperAudioProcessor::getNumPrograms()
{
    return 1;   // NB: some hosts don't cope very well if you tell them there are 0 programs,
                // so this should be at least 1, even if you're not really implementing programs.
}

int LukeperAudioProcessor::getCurrentProgram()
{
    return 0;
}

void LukeperAudioProcessor::setCurrentProgram (int index)
{
}

const String LukeperAudioProcessor::getProgramName (int index)
{
    return String();
}

void LukeperAudioProcessor::changeProgramName (int index, const String& newName)
{
}

//==============================================================================
void LukeperAudioProcessor::prepareToPlay (double sampleRate, int samplesPerBlock)
{
    // Use this method as the place to do any pre-playback
    // initialisation that you need..
}

void LukeperAudioProcessor::releaseResources()
{
    // When playback stops, you can use this as an opportunity to free up any
    // spare memory, etc.
}

#ifndef JucePlugin_PreferredChannelConfigurations
bool LukeperAudioProcessor::isBusesLayoutSupported (const BusesLayout& layouts) const
{
  #if JucePlugin_IsMidiEffect
    ignoreUnused (layouts);
    return true;
  #else
    // This is the place where you check if the layout is supported.
    // In this template code we only support mono or stereo.
    if (layouts.getMainOutputChannelSet() != AudioChannelSet::mono()
     && layouts.getMainOutputChannelSet() != AudioChannelSet::stereo())
        return false;

    // This checks if the input layout matches the output layout
   #if ! JucePlugin_IsSynth
    if (layouts.getMainOutputChannelSet() != layouts.getMainInputChannelSet())
        return false;
   #endif

    return true;
  #endif
}
#endif

void LukeperAudioProcessor::processBlock (AudioSampleBuffer& buffer, MidiBuffer& midiMessages)
{
    const int totalNumInputChannels  = getTotalNumInputChannels();
    const int totalNumOutputChannels = getTotalNumOutputChannels();

    // In case we have more outputs than inputs, this code clears any output
    // channels that didn't contain input data, (because these aren't
    // guaranteed to be empty - they may contain garbage).
    // This is here to avoid people getting screaming feedback
    // when they first compile a plugin, but obviously you don't need to keep
    // this code if your algorithm always overwrites all the output channels.


    if (_recording) {
        int i = 0;
        while(_loopBufIndex < _loopBuffer.size() && i < buffer.getNumSamples()) {
            _loopBuffer[_loopBufIndex++] = buffer.getSample(0, i++);
        }
        if (_loopBufIndex == _loopBuffer.size()) {
            _recording = false;
            _loopBufIndex = 0;
        }
    }
    else {
        int i = 0;
        while(_loopBufIndex < _loopBuffer.size() && i < buffer.getNumSamples()) {
            buffer.setSample(0, i++, _loopBuffer[_loopBufIndex++]);
        }
        if (_loopBufIndex == _loopBuffer.size()) {
            _recording = true;
            _loopBufIndex = 0;
        }
    }
    
    for (int i = (int)!_recording; i < totalNumOutputChannels; ++i)
        buffer.clear (i, 0, buffer.getNumSamples());
}

//==============================================================================
bool LukeperAudioProcessor::hasEditor() const
{
    return true; // (change this to false if you choose to not supply an editor)
}

AudioProcessorEditor* LukeperAudioProcessor::createEditor()
{
    return new LukeperAudioProcessorEditor (*this);
}

//==============================================================================
void LukeperAudioProcessor::getStateInformation (MemoryBlock& destData)
{
    // You should use this method to store your parameters in the memory block.
    // You could do that either as raw data, or use the XML or ValueTree classes
    // as intermediaries to make it easy to save and load complex data.
}

void LukeperAudioProcessor::setStateInformation (const void* data, int sizeInBytes)
{
    // You should use this method to restore your parameters from this memory block,
    // whose contents will have been created by the getStateInformation() call.
}

//==============================================================================
// This creates new instances of the plugin..
AudioProcessor* JUCE_CALLTYPE createPluginFilter()
{
    return new LukeperAudioProcessor();
}
