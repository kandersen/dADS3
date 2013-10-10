using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CreateAverage
{
    class Program
    {
        enum types
        {
            bha = 0,
            bhp = 1,
            fh1 = 2,
            fh2 = 3
        }

        static void Main(string[] args)
        {
            //ComputeFile(args);
            for (int i = 0; i <= 3; i++)
            {
                for (int j = 0; j <= 9; j++)
                {
                    string[] debugArgs = { "xdjk1.csv", string.Format("{0}_djk1_{1}_real.txt", ((types)i).ToString(), j), j.ToString(), i.ToString() };
                    ComputeFile(debugArgs);
                }
            }
        }

        private static void ComputeFile(string[] args)
        {
            string inputFile = args[0];
            string outputFile = args[1];
            int prob = int.Parse(args[2]);
            int version = int.Parse(args[3]);

            using (StreamWriter writer = new StreamWriter(File.OpenWrite(outputFile)))
            {
                int lastPoint = 0;
                List<int> results = new List<int>();

                using (System.IO.StreamReader sr = File.OpenText(inputFile))
                {
                    string line = null;

                    while ((line = sr.ReadLine()) != null)
                    {
                        string[] data = line.Split(' ');

                        int thisProb = int.Parse(data[1]);
                        int thisVersion = int.Parse(data[2]);
                        if (thisProb == prob && thisVersion == version)
                        {
                            int thisPoint = int.Parse(data[0]);
                            if (thisPoint != lastPoint && lastPoint > 0)
                            {
                                int edges = (lastPoint * lastPoint) * (1 - prob / 10);
                                ComputePoint(writer, lastPoint, edges, results);
                                results.Clear();
                            }

                            lastPoint = thisPoint;
                            results.Add(int.Parse(data[4]));
                        }
                    }
                }

                int edges2 = lastPoint * (1 - prob / 10);
                ComputePoint(writer, lastPoint, edges2, results);
            }
        }

        private static void ComputePoint(StreamWriter sw, int point, int edges, List<int> results)
        {
        //    double avg = (from x in results select x).Average() / (point * Math.Log(point, 2));
          //  sw.WriteLine(string.Format("({0},{1})", point, avg.ToString("N3").Replace(",",".")));

            double avg = (from x in results select x).Average();
            sw.WriteLine(string.Format("({0},{1})", point, ((int)avg).ToString()));
        }
    }
}
