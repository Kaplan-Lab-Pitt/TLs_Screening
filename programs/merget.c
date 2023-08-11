#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#define LIBRARY 0

int num_library [] =
{
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   1, 1, 1
};

int library2 [][2] =
{

   {2, 1}, {3, 1}, {4, 1}, {5, 2}, {6, 3}, {7, 4}, {8, 5}, {9, 6}, {10, 7},
   {11, 8}, {12, 9}, {13, 10}, {14, 8}, {15, 9}, {16, 10}, {17, 8}, {18, 9},
   {19, 10}, {20, 17}, {21, 18}, {22, 19}, {23, 8}, {24, 9}, {25, 10}, {26, 8},
   {27, 9}, {28, 10}
};

typedef struct mutation_node_type mutation_node;
struct mutation_node_type
{
   char *str;
   int *freq;
   mutation_node *next;
};

int mutation_compare (const void *mutation, const void *mutation2)
{
   return (strcmp ((*((mutation_node **) mutation)) -> str,
      (*((mutation_node **) mutation2)) -> str));
}

main (int argc, char **argv)
{
   FILE **file;
   int i, j, k, n, library, *status, *freq, *freq2, index, index2 [2], count,
      count2, found, pos;
   double score, score2 [2];
   char **str, buffer [1000000];
   mutation_node *mutation, *mutation1, **mutation2, **mutation3, **mutation4,
      *last_mutation, *current_mutation;

   if (argc < 2)
   {
      printf ("Usage: merget library [file]*\n");
      exit (-1);
   }
   library = atoi (argv [1]);
   if (library > argc - 2)
      library = LIBRARY;
   status = (int *) malloc ((argc - 2) * sizeof (int));
   if (errno == ENOMEM)
   {
      printf ("Error: Not enough memory for status\n");
      exit (-1);
   }
   for (i = 0; i < argc - 2; i++)
      status [i] = 2;
   freq = (int *) malloc ((argc - 2) * sizeof (int));
   if (errno == ENOMEM)
   {
      printf ("Error: Not enough memory for freq\n");
      exit (-1);
   }
   if (library != 0)
   {
      freq2 = (int *) malloc ((argc - 2) * sizeof (int));
      if (errno == ENOMEM)
      {
         printf ("Error: Not enough memory for freq2\n");
         exit (-1);
      }
      for (i = 0; i < argc - 2; i++)
         freq2 [i] = 0;
   }
   str = (char **) malloc ((argc - 2) * sizeof (char *));
   if (errno == ENOMEM)
   {
      printf ("Error: Not enough memory for str\n");
      exit (-1);
   }
   for (i = 0; i < argc - 2; i++)
   {
      str [i] = (char *) malloc (1000000 * sizeof (char));
      if (errno == ENOMEM)
      {
         printf ("Error: Not enough memory for str\n");
         exit (-1);
      }
      str [i][0] = '\0';
   }
   file = (FILE **) malloc ((argc - 2) * sizeof (FILE *));
   if (errno == ENOMEM)
   {
      printf ("Error: Not enough memory for file\n");
      exit (-1);
   }
   for (i = 0; i < argc - 2; i++)
   {
      file [i] = fopen (argv [i + 2], "r");
      if (file [i] == NULL)
      {
         printf ("Error: Cannot open file %s\n", argv [i + 2]);
         exit (-1);
      }
   }
   if (library != 0)
   {
      n = 0;
      mutation = NULL;
      mutation1 = (mutation_node *) malloc (sizeof (mutation_node));
      if (errno == ENOMEM)
      {
         printf ("Error: Not enough memory for mutation1\n");
         exit (-1);
      }
      mutation1 -> str = (char *) malloc (1000000 * sizeof (char));
      if (errno == ENOMEM)
      {
         printf ("Error: Not enough memory for str\n");
         exit (-1);
      }
      mutation1 -> freq = (int *) malloc ((argc - 2) * sizeof (int));
      if (errno == ENOMEM)
      {
         printf ("Error: Not enough memory for mutation1\n");
         exit (-1);
      }
      for (i = 0; i < argc - 2; i++)
         mutation1 -> freq [i] = 0;
      mutation2 = NULL;
   }
   index = argc - 3;
   while (1)
   {
      for (i = 0; i <= index; i++)
      {
         if ( (status [i] == 2)
           && ( ((str [index][0] != '\0') && (isdigit (str [i][0])))
             || (!strcmp (str [i], str [index]))) )
         {
            status [i] = fscanf (file [i], "%d%[^\n]", &freq [i], buffer);
            sscanf (buffer, "%s", str [i]);
         }
      }
      index = -1;
      for (i = 0; i < argc - 2; i++)
      {
         if ( (status [i] == 2)
           && ( (index < 0)
             || ((str [index][0] != '\0') && (isdigit (str [i][0])))
             || (strcmp (str [i], str [index]) <= 0) ) )
            index = i;
      }
      if (index < 0)
         break;
      if (library == 0)
      {
         if ( (index > 0) && (str [index][0] != '\0')
           && (!isupper (str [index][0])) )
         {
            count = 0;
            count2 = 0;
            for (i = 1; i < argc - 2; i++)
            {
               if ( (status [i] == 2)
                 && ( (isdigit (str [i][0]))
                   || (!strcmp (str [i], str [index])) ) )
               {
                  count += freq [i];
                  if (isdigit (str [i][0]))
                     count2 += atoi (str [i]);
               }
            }
            printf ("%d", count);
            if (count2 == 0)
            {
               printf ("\t%s", str [index]);
               count = 0;
               if (str [index][1] != ']')
               {
                  pos = 0;
                  do
                  {
                     sscanf (&str [index][pos + 1], "%[^],]", buffer);
                     pos += strlen (buffer) + 1;
                     count++;
                  }
                  while (str [index][pos] != ']');
               }
               printf ("\t%d\t", count);
               if (!strcmp (str [0], str [index]))
                  printf ("E");
               else
                  printf ("U");
            }
            else
               printf ("/%d\t\t\t", count2);
            for (i = 1; i < argc - 2; i++)
            {
               printf ("\t");
               if ( (status [i] == 2)
                 && ( (isdigit (str [i][0]))
                   || (!strcmp (str [i], str [index])) ) )
               {
                  printf ("%d", freq [i]);
                  if (isdigit (str [i][0]))
                     printf ("/%s", str [i]);
               }
            }
            printf ("\n");
         }
      }
      else if ( (library < 0)
             || ( (status [library - 1] == 2)
               && ( ( (str [index][0] != '\0')
                   && (isdigit (str [library - 1][0])) )
                 || (!strcmp (str [library - 1], str [index])) ) ) )
      {
         if (isupper (str [index][0]))
         {
            current_mutation = (mutation_node *)
               malloc (sizeof (mutation_node));
            if (errno == ENOMEM)
            {
               printf ("Error: Not enough memory for current_mutation\n");
               exit (-1);
            }
            current_mutation -> str = (char *)
               malloc ((strlen (str [index]) + 1) * sizeof (char));
            if (errno == ENOMEM)
            {
               printf ("Error: Not enough memory for str\n");
               exit (-1);
            }
            strcpy (current_mutation -> str, str [index]);
            current_mutation -> freq = (int *)
               malloc ((argc - 2) * sizeof (int));
            if (errno == ENOMEM)
            {
               printf ("Error: Not enough memory for freq\n");
               exit (-1);
            }
            for (i = 0; i < argc - 2; i++)
               current_mutation -> freq [i] = 0;
            current_mutation -> next = NULL;
            if (mutation == NULL)
               mutation = current_mutation;
            else
               last_mutation -> next = current_mutation;
            last_mutation = current_mutation;
            n++;
         }
         else if (str [index][0] == '[')
         {
            if (mutation2 == NULL)
            {
               mutation2 = (mutation_node **)
                  malloc (n * sizeof (mutation_node *));
               if (errno == ENOMEM)
               {
                  printf ("Error: Not enough memory for mutation2\n");
                  exit (-1);
               }
               for (i = 0; i < n; i++)
               {
                  mutation2 [i] = mutation;
                  mutation = mutation -> next;
               }
               qsort (mutation2, n, sizeof (mutation_node *), mutation_compare);
            }
            count = 0;
            found = 0;
            if (str [index][1] != ']')
            {
               pos = 0;
               do
               {
                  sscanf (&str [index][pos + 1], "%[^],]", mutation1 -> str);
                  if (library > 0)
                  {
                     mutation3 = (mutation_node **) bsearch (&mutation1,
                        mutation2, n, sizeof (mutation_node *),
                        mutation_compare);
                     if ( (mutation3 == NULL)
                       || ((*mutation3) -> freq [library - 1] == 0) )
                     {
                        found = 1;
                        break;
                     }
                  }
                  pos += strlen (mutation1 -> str) + 1;
                  count++;
               }
               while (str [index][pos] != ']');
            }
            if ((!found) && ((library > 0) || (index > 0)))
            {
               if (library > 0)
                  printf ("%d", freq [library - 1]);
               else
               {
                  count2 = 0;
                  for (i = 1; i < argc - 2; i++)
                  {
                     if ((status [i] == 2) && (!strcmp (str [i], str [index])))
                        count2 += freq [i];
                  }
                  printf ("%d", count2);
               }
               printf ("\t%s\t%d\t", str [index], count);
               if (!strcmp (str [0], str [index]))
                  printf ("E");
               else
                  printf ("U");
            }
         } 
         for (i = 0; i < argc - 2; i++)
         {
            if ((library > 0) && (str [index][0] == '[') && (!found))
               printf ("\t");
            if ( (status [i] == 2)
              && ( ((str [index][0] != '\0') && (isdigit (str [i][0])))
                || (!strcmp (str [i], str [index]))) )
            {
               if (str [i][0] == '\0')
                  freq2 [i] = freq [i];
               else if (isupper (str [i][0]))
                  current_mutation -> freq [i] = freq [i];
               else if ((library > 0) && (str [i][0] == '[') && (!found))
               {
                  sscanf (str [i], "%[^],]", buffer);
                  if (str [i][strlen (buffer)] == ']')
                     printf ("%lf", log (1.0 * freq [i] / freq [library - 1]
                        * freq2 [library - 1] / freq2 [i]) / log (2.0));
                  else
                  {
                     score = 1.0 * freq [i] / freq [library - 1]
                        * freq2 [library - 1] / freq2 [i];
                     pos = 0;
                     do
                     {
                        sscanf (&str [i][pos + 1], "%[^],]", mutation1 -> str);
                        mutation3 = (mutation_node **) bsearch (&mutation1,
                           mutation2, n, sizeof (mutation_node *),
                           mutation_compare);
                        if ( (mutation3 == NULL)
                          || ((*mutation3) -> freq [i] == 0) )
                           break;
                        score /= 1.0 * (*mutation3) -> freq [i]
                           / (*mutation3) -> freq [library - 1]
                           * freq2 [library - 1] / freq2 [i];
                        pos += strlen (mutation1 -> str) + 1;
                     }
                     while (str [i][pos] != ']');
                     if ((mutation3 != NULL) && ((*mutation3) -> freq [i] > 0))
                        printf ("%lf", log (score) / log (2.0));
                  }
               }
            }
         }
         if ((str [index][0] == '[') && (!found))
         {
            if (library > 0)
               printf ("\n");
            else
            {
               for (i = 0; i < sizeof (library2) / sizeof (int *); i++)
               {
                  printf ("\t");
                  found = 0;
                  sscanf (str [index], "%[^],]", buffer);
                  for (j = 0; j < 2; j++)
                  {
                     index2 [j] = 0;
                     score2 [j] = 0.0;
                     for (k = 0; k < num_library [library2 [i][j]]; k++)
                     {
                        if ( (status [library2 [i][j] + k] == 2)
                          && (!strcmp (str [library2 [i][j] + k],
                                 str [index])) )
                           found = 1;
                        if (freq2 [library2 [i][j] + k] - library - 1 > 0)
                        {
                           if (str [index][strlen (buffer)] == ']')
                           {
                              if ( (status [library2 [i][j] + k] == 2)
                                && (!strcmp (str [library2 [i][j] + k],
                                       str [index])) )
                                 score2 [j] += 1.0 * (freq [library2 [i][j] + k]
                                    - library - 1) / (freq2 [library2 [i][j]
                                    + k] - library - 1);
                              else
                                 score2 [j] += 1.0 * (- library - 1) / (freq2
                                    [library2 [i][j] + k] - library - 1);
                           }
                           else
                           {
                              if ( (status [library2 [i][j] + k] == 2)
                                && (!strcmp (str [library2 [i][j] + k],
                                       str [index])) )
                                 score = 1.0 * (freq [library2 [i][j] + k]
                                    - library - 1) / (freq2 [library2 [i][j]
                                    + k] - library - 1);
                              else
                                 score = 1.0 * (- library - 1) / (freq2
                                    [library2 [i][j] + k] - library - 1);
                              pos = 0;
                              do
                              {
                                 sscanf (&str [index][pos + 1], "%[^],]",
                                    mutation1 -> str);
                                 mutation3 = (mutation_node **)
                                    bsearch (&mutation1, mutation2, n,
                                    sizeof (mutation_node *), mutation_compare);
                                 if (mutation3 == NULL)
                                    mutation3 = &mutation1;
                                 pos += strlen (mutation1 -> str) + 1;
                                 strcpy (mutation1 -> str, "A");
                                 mutation4 = (mutation_node **)
                                    bsearch (&mutation1, mutation2, n,
                                    sizeof (mutation_node *), mutation_compare);
                                 if ((*mutation3) -> freq [library2 [i][j] + k]
                                        - library - 1 > 0)
                                 {
                                    if (mutation4 == NULL)
                                       score /= 1.0 * ((*mutation3) -> freq
                                          [library2 [i][j] + k] - library - 1)
                                          / (freq2 [library2 [i][j] + k]
                                          - library - 1);
                                    else if ((*mutation4) -> freq [library2 [i]
                                                [j] + k] - library - 1 > 0)
                                       score /= 1.0 * ((*mutation3) -> freq
                                          [library2 [i][j] + k] - library - 1)
                                          / ((*mutation4) -> freq [library2 [i]
                                          [j] + k] - library - 1);
                                 }
                              }
                              while (str [index][pos] != ']');
                              if ( ((*mutation3) -> freq [library2 [i][j] + k]
                                       - library - 1 > 0)
                                && ( (mutation4 == NULL)
                                  || ((*mutation4) -> freq [library2 [i][j] + k]
                                         - library - 1 > 0) ) )
                                 score2 [j] += score;
                           }
                           index2 [j]++;
                        }
                     }
                  }
                  if (found && (score2 [0] > 0.0) && (score2 [1] > 0.0))
                     printf ("%lf", log (score2 [0] / index2 [0] / score2 [1]
                        * index2 [1]) / log (2.0));
               }
               printf ("\n");
            }
         }
      }
   }
   free (status);
   free (freq);
   if (library != 0)
      free (freq2);
   for (i = 0; i < argc - 2; i++)
      free (str [i]);
   free (str);
   for (i = 0; i < argc - 2; i++)
   {
      if (fclose (file [i]) == EOF)
      {
         printf ("Error: Cannot close file %s\n", argv [i + 1]);
         exit (-1);
      }
   }
   free (file);
   if (library != 0)
   {
      free (mutation1 -> str);
      free (mutation1 -> freq);
      free (mutation1);
      if (mutation2 != NULL)
      {
         for (i = 0; i < n; i++)
         {
            free (mutation2 [i] -> str);
            free (mutation2 [i] -> freq);
            free (mutation2 [i]);
         }
         free (mutation2);
      }
   }
}
