#include <stdio.h>
#include <stdlib.h>

main (int argc, char **argv)
{
   FILE *file;
   int i;
   char buffer [1000000];

   if (argc != 3)
   {
      printf ("Usage: merges0 file1 file2\n");
      exit (-1);
   }
   for (i = 0; i < 2; i++)
   {
      file = fopen (argv [i + 1], "r");
      if (file == NULL)
      {
         printf ("Error: Cannot open file %s\n", argv [i + 1]);
         exit (-1);
      }
      fgets (buffer, 1000000, file);
      if (i == 0)
         printf ("%s", buffer);
      else
      {
         while (fgets (buffer, 1000000, file) != NULL)
            printf ("%s", buffer);
      }
      if (fclose (file) == EOF)
      {
         printf ("Error: Cannot close file %s\n", argv [i + 1]);
         exit (-1);
      }
   }
}
