/*
  ==============================================================================

    This file was auto-generated!

    It contains the basic framework code for a JUCE plugin editor.

  ==============================================================================
*/

#include "PluginProcessor.h"
#include "PluginEditor.h"


extern "C" {
    int foo(int);
}

//==============================================================================
LukeperAudioProcessorEditor::LukeperAudioProcessorEditor (LukeperAudioProcessor& p)
    : AudioProcessorEditor (&p), _processor (p)
{
    // Make sure that before the constructor has finished, you've set the
    // editor's size to whatever you need it to be.
    setSize (400, 300);
}

LukeperAudioProcessorEditor::~LukeperAudioProcessorEditor()
{
}

//==============================================================================
void LukeperAudioProcessorEditor::paint (Graphics& g)
{
    g.fillAll (Colours::white);

    /*
    g.setColour (Colours::black);
    g.setFont (15.0f);
    if (_processor.recording()) {
        juce::String s("recording: ");
        s += foo(41);
        s += _processor.loopBufIndex();
        g.drawFittedText (s, getLocalBounds(), Justification::centred, 1);
    }
    else {
        juce::String s("playing: ");
        s += foo(41);
        s += _processor.loopBufIndex();
        g.drawFittedText (s, getLocalBounds(), Justification::centred, 1);
    }
    repaint();
    */
}

void LukeperAudioProcessorEditor::resized()
{
    // This is generally where you'll want to lay out the positions of any
    // subcomponents in your editor..
}
