using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace CreateAverage
{
    class Program
    {
        static void Main(string[] args)
        {
            string inputFile = args[0];
            string outputFile = args[1];
            int prob = int.Parse(args[2]);

            using (StreamWriter writer = new StreamWriter(File.OpenWrite(outputFile)))
            {
                int lastPoint = 0;
                List<int> results = new List<int>();

                using (System.IO.StreamReader sr = File.OpenText(inputFile))
                {
                    string line = null;
                        
                    while ((line = sr.ReadLine()) != null) {
                        string[] data = line.Split('\t');

                        int thisProb = int.Parse(data[1]);
                        if (thisProb == prob)
                        {
                            int thisPoint = int.Parse(data[0]);
                            if (thisPoint != lastPoint && lastPoint > 0)
                            {
                                int edges = lastPoint * (1 - prob / 10);
                                ComputePoint(writer, lastPoint, edges, results);
                                results.Clear();
                            }

                            lastPoint = thisPoint;
                            results.Add(int.Parse(data[5]));
                        }
                    }
                }

                int edges2 = lastPoint * (1 - prob / 10);
                ComputePoint(writer, lastPoint, edges2, results);
            }
        }

        private static void ComputePoint(StreamWriter sw, int point, int edges, List<int> results)
        {
            double avg = (from x in results select x).Average() / ((point + edges) * Math.Log(point, 2));
            sw.WriteLine(string.Format("({0},{1})", point, avg.ToString("N3").Replace(",",".")));
        }
    }
}
