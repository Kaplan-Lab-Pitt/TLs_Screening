#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

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

main ()
{
   int i, j, num_sequence, found, frame, len_sequence, len_sequence_name;
   char *str, **sequence, **sequence_name;

   str = fmakeword (stdin, EOF, NULL);
   sequence = read_fasta (str, &sequence_name, &num_sequence);
   free (str);
   found = 0;
   frame = 0;
   for (i = 0; i < num_sequence; i++)
   {
      if (i == 0)
      {
         len_sequence = strlen (sequence [i]);
         for (j = 0; j < len_sequence; j++)
         {
            if (isupper (sequence [i][j]))
            {
               frame = j % 3;
               break;
            }
         }
      }
      if (strncmp (sequence_name [i], "WT", strlen ("WT")))
      {
         if (strstr (sequence_name [i], "WT") != NULL)
         {
            printf (">WT\n");
            len_sequence = strlen (sequence [i]);
            for (j = frame; j < len_sequence - (len_sequence - frame) % 3 - 3;
                 j++)
               printf ("%c", sequence [i][j]);
            printf ("\n");
            found = 1;
         }
         if (found)
         {
            printf (">");
            if (strstr (sequence_name [i], "WT") != NULL)
               printf ("WT");
            else
            {
               len_sequence_name = strlen (sequence_name [i]);
               for (j = 0; j < len_sequence_name; j++)
               {
                  if (sequence_name [i][j] == ' ')
                     break;
                  printf ("%c", sequence_name [i][j]);
               }
            }
            printf ("\n%s\n", sequence [i]);
         }
      }
      free (sequence [i]);
      free (sequence_name [i]);
   }
   free (sequence);
   free (sequence_name);
}
