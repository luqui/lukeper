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
    : AudioProcessorEditor (&p), _processor (p), _paintcalls(0)
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

    g.setColour (Colours::black);
    g.setFont (15.0f);
    g.drawFittedText(_processor.getLooper().uilog(), getLocalBounds(), Justification::centred, 30);
    repaint();
}

void LukeperAudioProcessorEditor::resized()
{
    // This is generally where you'll want to lay out the positions of any
    // subcomponents in your editor..
}
