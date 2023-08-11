#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define min(a,b) ((a < b) ? a : b)

char *fmakeword(FILE *f, char stop, int *cl) {
    int wsize;
    char *word;
    int ll;

    wsize = 102400;
    ll=0;
    word = (char *) malloc(sizeof(char) * (wsize + 2));

    while(1) {
        word[ll] = (char)fgetc(f);
        if(ll==wsize) {
            word[ll+1] = '\0';
            wsize+=102400;
            word = (char *)realloc(word,sizeof(char)*(wsize+2));
        }
        if (cl != NULL)
           --(*cl);
        if((word[ll] == stop) || (feof(f)) || ((cl != NULL) && (!(*cl)))) {
            if((word[ll] != stop) && (!feof(f)) && ((cl == NULL) || (!(*cl))))
                ll++;
            word[ll] = '\0';
            return word;
        }
        ++ll;
    }
}

char **read_fasta (char *str, char ***sequence_name, int *num_sequence)
{
   int i, n, start, end, len_sequence;
   char **sequence;

   n = strlen (str);
   (*num_sequence) = 0;
   i = 0;
   while (i < n)
   {
      while ((i < n) && (str [i] != '>'))
         i++;
      while ((i < n) && (str [i] != '\n'))
         i++;
      while ( (i < n) && (str [i] != '>') && (!islower (str [i]))
           && (!isupper (str [i])) )
         i++;
      (*num_sequence)++;
   }
   sequence = (char **) malloc ((*num_sequence) * sizeof (char *));
   if (errno == ENOMEM)
   {
      printf ("Error: Not enough memory for sequence\n");
      exit (-1);
   }
   (*sequence_name) = (char **) malloc ((*num_sequence) * sizeof (char *));
   if (errno == ENOMEM)
   {
      printf ("Error: Not enough memory for sequence_name\n");
      exit (-1);
   }
   (*num_sequence) = 0;
   i = 0;
   while ((i < n) && (str [i] != '>'))
      i++;
   while (i < n)
   {
      start = i + 1;
      while ((i < n) && (str [i] != '\n'))
         i++;
      end = i - 1;
      while ( (i < n) && (str [i] != '>') && (!islower (str [i]))
           && (!isupper (str [i])) )
         i++;
      (*sequence_name) [*num_sequence] = (char *)
         malloc ((end - start + 2) * sizeof (char));
      if (errno == ENOMEM)
      {
         printf ("Error: Not enough memory for sequence_name\n");
         exit (-1);
      }
      strncpy ((*sequence_name) [*num_sequence], &str [start], end - start + 1);
      (*sequence_name) [*num_sequence][end - start + 1] = '\0';
      start = i;
      len_sequence = 0;
      while ((i < n) && (str [i] != '>'))
      {
         if ((islower (str [i])) || (isupper (str [i])))
            len_sequence++;
         i++;
      }
      sequence [*num_sequence] = (char *)
         malloc ((len_sequence + 1) * sizeof (char));
      if (errno == ENOMEM)
      {
         printf ("Error: Not enough memory for sequence\n");
         exit (-1);
      }
      i = start;
      len_sequence = 0;
      while ((i < n) && (str [i] != '>'))
      {
         if ((islower (str [i])) || (isupper (str [i])))
         {
            sequence [*num_sequence][len_sequence] = str [i];
            len_sequence++;
         }
         i++;
      }
      sequence [*num_sequence][len_sequence] = '\0';
      (*num_sequence)++;
   }
   return sequence;
}

main (int argc, char **argv)
{
   FILE *file;
   int i, j, k, found, num_sequence, frame, len_sequence, len_sequence2,
      len_sequence_name;
   char sequence_name [1000000], **sequence_name2, sequence [1000000],
      **sequence2, *str;

   found = 0;
   for (i = 0; i < argc - 1; i++)
   {
      file = fopen (argv [i + 1], "r");
      if (file == NULL)
      {
         printf ("Error: Cannot open file %s\n", argv [i + 1]);
         exit (-1);
      }
      while (fscanf (file, "%[^\n] %s %*[^\n] %*s\n", sequence_name, sequence)
                == 2)
      {
         if (!found)
         {
            len_sequence = strlen (sequence);
            str = fmakeword (stdin, EOF, NULL);
            sequence2 = read_fasta (str, &sequence_name2, &num_sequence);
            free (str);
            frame = 0;
            for (j = 0; j < num_sequence; j++)
            {
               if (j == 0)
               {
                  len_sequence2 = strlen (sequence2 [j]);
                  for (k = 0; k < len_sequence2; k++)
                  {
                     if (isupper (sequence2 [j][k]))
                     {
                        frame = k % 3;
                        break;
                     }
                  }
               }
               if (strstr (sequence_name2 [j], "WT") != NULL)
               {
                  printf (">WT\n");
                  len_sequence2 = strlen (sequence2 [j]);
                  for (k = frame; k < min (len_sequence, len_sequence2)
                          - (min (len_sequence, len_sequence2) - frame) % 3;
                       k++)
                     printf ("%c", sequence2 [j][k]);
                  printf ("\n");
                  break;
               }
            }
            for (j = 0; j < num_sequence; j++)
               free (sequence2 [j]);
            free (sequence2);
            for (j = 0; j < num_sequence; j++)
               free (sequence_name2 [j]);
            free (sequence_name2);
            found = 1;
         }
         printf (">");
         len_sequence_name = strlen (sequence_name);
         for (j = 1; j < len_sequence_name; j++)
         {
            if (sequence_name [j] == ' ')
               break;
            printf ("%c", sequence_name [j]);
         }
         printf ("\n%s\n", sequence);
      }
      if (fclose (file) == EOF)
      {
         printf ("Error: Cannot close file %s\n", argv [i + 1]);
         exit (-1);
      }
   }
}
