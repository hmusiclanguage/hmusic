import java.io.File;
import java.io.IOException;

import com.jsyn.JSyn;
import com.jsyn.Synthesizer;
import com.jsyn.data.FloatSample;
import com.jsyn.unitgen.LineOut;
import com.jsyn.unitgen.VariableRateDataReader;
import com.jsyn.unitgen.VariableRateMonoReader;
import com.jsyn.unitgen.VariableRateStereoReader;
import com.jsyn.util.SampleLoader;

public class Backend {
  private Synthesizer synth;
  private LineOut lineOut;
  /*
  * NOTE: the number of channels should be independent from the
  * number of actual samples and instead be equivalent to the
  * number of tracks in the HMusic song.
  */
  private VariableRateDataReader[] channels;
  private FloatSample[] samples;

  public Backend()
  {
    synth = JSyn.createSynthesizer();
    synth.add(lineOut = new LineOut());
  }

  public Backend loadSamples (String[] paths) throws IOException
  {
    /* Load the samples from their files. */
    SampleLoader.setJavaSoundPreferred(false);
    samples = new FloatSample[paths.length];
    for (int i = 0; i < paths.length; i++) {
      File file = new File(paths[i]);
      samples[i] = SampleLoader.loadFloatSample(file);
    }

    /* Create a channel for each sample. */
    channels = new VariableRateDataReader[paths.length];
    for (int i = 0; i < samples.length; i++) {
      if (samples[i].getChannelsPerFrame() == 1) {
        synth.add(channels[i] = new VariableRateMonoReader());
        channels[i].output.connect(0, lineOut.input, 0);
      } else if (samples[i].getChannelsPerFrame() == 2) {
        synth.add(channels[i] = new VariableRateStereoReader());
        channels[i].output.connect(0, lineOut.input, 0);
        channels[i].output.connect(1, lineOut.input, 1);
      } else {
        throw new RuntimeException("Can only play mono or stereo samples.");
      }

      channels[i].rate.set(samples[i].getFrameRate());
    }

    return this;
  }

  public Backend play (double bpm, int[][] pattern, boolean loop)
  {
    double songRate;

    /* Always 60 / BPM. You can think of it as seconds between each beat. */
    songRate = 60.0 / bpm;

    try {
      synth.start();
      lineOut.start();

      double time = synth.getCurrentTime();
      do {
        for (int[] beat : pattern) {  /* For each beat in a pattern, */
          for (int sample : beat) { /* play all the samples in that beat. */
            channels[sample].dataQueue.queue(samples[sample],
                                            0,
                                            /* Sample cutoff at the start of next beat. */
                                            (Math.min((int) (samples[sample].getFrameRate() * songRate),
                                                      samples[sample].getNumFrames())));
          }
          /* Then wait until next beat. */
          time += songRate;
          synth.sleepUntil(time);
        }
      } while (loop);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    synth.stop();

    return this;
  }

  /* Testing main. */
  public static void main (String[] args)
  {
    try {
      new Backend()
        .loadSamples(new String[] {"sample/bd.wav", "sample/sd.wav", "sample/hh.wav"})
        .play(120, new int[][] {{0, 2}, {0, 2}, {1, 2}, {2}}, true);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
