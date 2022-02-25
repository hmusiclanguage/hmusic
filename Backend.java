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
  /* Constants. */
  /* Always 60 / BPM. You can think of it as seconds between each beat. */
  private static final double songRate = 60.0 / 120.0;
  private static final String[] sampleFiles = {
    "sample/bd.wav", "sample/sd.wav", "sample/hh.wav"
  };
  private static final boolean loop = true; /* Whether song loops or not. */

  /* Thinking not constant because it might change with code,
  * but that might be computed at compile time maybe. */
  private static int[][] pattern = {{0, 2}, {}, {1, 2}, {2}};

  public static void main (String[] args)
  {
    Synthesizer synth;
    LineOut lineOut;
    /*
    * NOTE: the number of channels should be independent from the
    * number of actual samples and instead be equivalent to the
    * number of tracks in the HMusic song.
    */
    VariableRateDataReader[] channels;
    FloatSample[] samples;

    synth = JSyn.createSynthesizer();

    try {
      synth.add(lineOut = new LineOut());

      /* Load the samples from their files. */
      SampleLoader.setJavaSoundPreferred(false);
      samples = new FloatSample[sampleFiles.length];
      for (int i = 0; i < sampleFiles.length; i++) {
        File file = new File(sampleFiles[i]);
        samples[i] = SampleLoader.loadFloatSample(file);
      }

      /* Create a channel for each sample. */
      channels = new VariableRateDataReader[sampleFiles.length];
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
    } catch (IOException e) {
      e.printStackTrace();
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    synth.stop();
  }
}
