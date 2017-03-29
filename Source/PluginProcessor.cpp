/*
  ==============================================================================

    This file was auto-generated!

    It contains the basic framework code for a JUCE plugin processor.

  ==============================================================================
*/

#include "PluginProcessor.h"
#include "PluginEditor.h"


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
#endif
{ }

LukeperAudioProcessor::~LukeperAudioProcessor()
{ }

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
    for (int i = totalNumInputChannels; i < totalNumOutputChannels; ++i)
        buffer.clear (i, 0, buffer.getNumSamples());

    MarshalMidiMessage* in_messages = (MarshalMidiMessage*)malloc(midiMessages.getNumEvents()*sizeof(MarshalMidiMessage));
    MidiBuffer::Iterator iter(midiMessages);
    MidiMessage msg;
    int samplePos;
    int msg_count = 0;
    int ignore_count = 0;
    while (iter.getNextEvent(msg, samplePos)) {
        int dataBytes = msg.getRawDataSize();
        const uint8* data = msg.getRawData();
        if (dataBytes <= 3) {
            in_messages[msg_count].samplePosition = samplePos;
            in_messages[msg_count].message[0] = dataBytes >= 1 ? data[0] : 0;
            in_messages[msg_count].message[1] = dataBytes >= 2 ? data[1] : 0;
            in_messages[msg_count].message[2] = dataBytes >= 3 ? data[2] : 0;
        }
        else {
            ignore_count++;
        }
        msg_count++;
    }

    uint32 out_msg_count = 0;
    MarshalMidiMessage* out_messages = _hslooper.process_samples(
        buffer.getNumSamples(), totalNumInputChannels, totalNumOutputChannels, 
        buffer.getArrayOfWritePointers(), msg_count - ignore_count, in_messages, &out_msg_count);
    free(in_messages);

    // Apparently this does not actually filter midi messages, they are just getting passed through.
    midiMessages.clear();
    /*
    midiMessages.ensureSize(9*out_msg_count);
    for (int i = 0; i < out_msg_count; i++) {
        switch (MidiMessage::getMessageLengthFromFirstByte(out_messages[i].message[0])) {
            case 1:
                midiMessages.addEvent(MidiMessage(out_messages[i].message[0]), out_messages[i].samplePosition);
                break;
            case 2: 
                midiMessages.addEvent(MidiMessage(out_messages[i].message[0], out_messages[i].message[1]), out_messages[i].samplePosition);
                break;
            case 3: 
                midiMessages.addEvent(MidiMessage(out_messages[i].message[0], out_messages[i].message[1], out_messages[i].message[2]), out_messages[i].samplePosition); 
                break;
        }
    }
    */
    free(out_messages);
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
