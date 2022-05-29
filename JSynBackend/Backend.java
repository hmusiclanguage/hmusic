package JSynBackend;

import java.io.File;
import java.io.IOException;

import com.jsyn.JSyn;
import com.jsyn.Synthesizer;
import com.jsyn.data.FloatSample;
import com.jsyn.data.Function;
import com.jsyn.unitgen.FunctionEvaluator;
import com.jsyn.unitgen.LineOut;
import com.jsyn.unitgen.VariableRateDataReader;
import com.jsyn.unitgen.VariableRateMonoReader;
import com.jsyn.unitgen.VariableRateStereoReader;
import com.jsyn.unitgen.UnitGenerator;
import com.jsyn.util.SampleLoader;

public class Backend {
  private Synthesizer synth;
  private LineOut lineOut;
  /*
   * TODO: The number of channels should be independent from the
   * number of actual samples and instead be equal to the amount of
   * tracks in the song.
   */
  private VariableRateDataReader[] channels;
  private FloatSample[] samples;

  public Backend ()
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
      File file = new File("sample/" + paths[i] + ".wav");
      samples[i] = SampleLoader.loadFloatSample(file);
    }

    /* Create a channel for each sample. */
    channels = new VariableRateDataReader[paths.length];
    for (int i = 0; i < samples.length; i++) {
      if (samples[i].getChannelsPerFrame() == 1) {
        synth.add(channels[i] = new VariableRateMonoReader());
        channels[i].output.connect(0, lineOut.input, 0);
        channels[i].output.connect(0, lineOut.input, 1);
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

  public Backend attachEffect (int i, Function f)
  {
    FunctionEvaluator unit = new FunctionEvaluator();

    /* Create the function evaluator unit. */
    synth.add(unit);
    unit.function.set(f);

    /* Detach from line and attach to effect. */
    if (samples[i].getChannelsPerFrame() == 2) {
      channels[i].output.disconnect(0, lineOut.input, 0);
      channels[i].output.disconnect(1, lineOut.input, 1);

      /* Problem: No stereo in effects rn. */
      channels[i].output.connect(0, unit.input, 0);
      channels[i].output.connect(1, unit.input, 0);

      unit.output.connect(0, lineOut.input, 0);
      unit.output.connect(0, lineOut.input, 1);
    } else {
      channels[i].output.disconnect(0, lineOut.input, 0);

      channels[i].output.connect(0, unit.input, 0);

      unit.output.connect(0, lineOut.input, 0);
      unit.output.connect(0, lineOut.input, 1);
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

      synth.sleepFor(0.1);

      double time = synth.getCurrentTime();
      do {
        for (int[] beat : pattern) {  /* For each beat in a pattern, */
          for (int sample : beat) {   /* play all the samples in that beat. */
            /*
             * TODO: decouple channels from samples and instead queue on
             * whichever channels are free (maybe have a counter for how many
             * channels are used, so i = 0 would queue on the first channel,
             * up to i = chnamt - 1).
             */
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
