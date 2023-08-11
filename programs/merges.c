#include <stdio.h>
#include <stdlib.h>
#include <string.h>

main (int argc, char **argv)
{
   FILE *file;
   int i, j;
   char buffer [1000000], str [1000000];

   if (argc != 3)
   {
      printf ("Usage: merges file1 file2\n");
      exit (-1);
   }
   for (i = 0; i < 3; i++)
   {
      file = fopen (argv [i % 2 + 1], "r");
      if (file == NULL)
      {
         printf ("Error: Cannot open file %s\n", argv [i + 1]);
         exit (-1);
      }
      if (i == 0)
      {
         for (j = 0; j < 2; j++)
         {
            fgets (buffer, 1000000, file);
            printf ("%s", buffer);
         }
      }
      else if (i == 1)
      {
         fgets (str, 1000000, file);
         str [strlen (str) - 1] = '\0';
         fgets (buffer, 1000000, file);
         printf ("%sA\n", str);
         while (fgets (buffer, 1000000, file) != NULL)
         {
            sscanf (buffer, "%*s %s", str);
            if (isupper (str [0]))
               printf ("%s", buffer);
         }
      }
      else
      {
         for (j = 0; j < 2; j++)
            fgets (buffer, 1000000, file);
         while (fgets (buffer, 1000000, file) != NULL)
         {
            sscanf (buffer, "%*s %s", str);
            if (str [0] == '[')
               printf ("%s", buffer);
         }
      }
      if (fclose (file) == EOF)
      {
         printf ("Error: Cannot close file %s\n", argv [i % 2 + 1]);
         exit (-1);
      }
   }
}
