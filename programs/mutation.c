#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define CODON_INDEX 1
#define CODON_START 1010
#define REGION_START 1076
#define REGION_END 1106
#define MATCH 1
#define MISMATCH -1
#define INDEL -3

#define INFINITY 1000000

#define min(a,b) ((a < b) ? a : b)
#define max(a,b) ((a > b) ? a : b)
#define nucleotide(c) ((c=='a')?0:((c=='c')?1:((c=='g')?2:((c=='t')?3:-1))))

typedef struct char_node_type char_node;
struct char_node_type
{
   char ch;
   char_node *next;
};

char amino_acid [] =
{
   /* aaa */ 'k', /* aac */ 'n', /* aag */ 'k', /* aat */ 'n',
   /* aca */ 't', /* acc */ 't', /* acg */ 't', /* act */ 't',
   /* aga */ 'r', /* agc */ 's', /* agg */ 'r', /* agt */ 's',
   /* ata */ 'i', /* atc */ 'i', /* atg */ 'm', /* att */ 'i',
   /* caa */ 'q', /* cac */ 'h', /* cag */ 'q', /* cat */ 'h',
   /* cca */ 'p', /* ccc */ 'p', /* ccg */ 'p', /* cct */ 'p',
   /* cga */ 'r', /* cgc */ 'r', /* cgg */ 'r', /* cgt */ 'r',
   /* cta */ 'l', /* ctc */ 'l', /* ctg */ 'l', /* ctt */ 'l',
   /* gaa */ 'e', /* gac */ 'd', /* gag */ 'e', /* gat */ 'd',
   /* gca */ 'a', /* gcc */ 'a', /* gcg */ 'a', /* gct */ 'a',
   /* gga */ 'g', /* ggc */ 'g', /* ggg */ 'g', /* ggt */ 'g',
   /* gta */ 'v', /* gtc */ 'v', /* gtg */ 'v', /* gtt */ 'v',
   /* taa */ '.', /* tac */ 'y', /* tag */ '.', /* tat */ 'y',
   /* tca */ 's', /* tcc */ 's', /* tcg */ 's', /* tct */ 's',
   /* tga */ '.', /* tgc */ 'c', /* tgg */ 'w', /* tgt */ 'c',
   /* tta */ 'l', /* ttc */ 'f', /* ttg */ 'l', /* ttt */ 'f'
};

int codon [] =
{
   /* aaa */ 1, /* aac */ 0, /* aag */ 0, /* aat */ 1,
   /* aca */ 0, /* acc */ 0, /* acg */ 0, /* act */ 1,
   /* aga */ 1, /* agc */ 0, /* agg */ 0, /* agt */ 0,
   /* ata */ 0, /* atc */ 0, /* atg */ 1, /* att */ 1,
   /* caa */ 1, /* cac */ 0, /* cag */ 0, /* cat */ 1,
   /* cca */ 1, /* ccc */ 0, /* ccg */ 0, /* cct */ 0,
   /* cga */ 0, /* cgc */ 0, /* cgg */ 0, /* cgt */ 0,
   /* cta */ 0, /* ctc */ 0, /* ctg */ 0, /* ctt */ 0,
   /* gaa */ 1, /* gac */ 0, /* gag */ 0, /* gat */ 1,
   /* gca */ 0, /* gcc */ 0, /* gcg */ 0, /* gct */ 1,
   /* gga */ 0, /* ggc */ 0, /* ggg */ 0, /* ggt */ 1,
   /* gta */ 0, /* gtc */ 0, /* gtg */ 0, /* gtt */ 1,
   /* taa */ 0, /* tac */ 0, /* tag */ 0, /* tat */ 1,
   /* tca */ 0, /* tcc */ 0, /* tcg */ 0, /* tct */ 1,
   /* tga */ 0, /* tgc */ 0, /* tgg */ 1, /* tgt */ 1,
   /* tta */ 0, /* ttc */ 0, /* ttg */ 1, /* ttt */ 1
};

main (int argc, char **argv)
{
   int i, j, k, codon_index, codon_start, region_start, region_end, match,
      mismatch, indel, len_str, found, found2, found3, len_sequence [2],
      **score, **move, **move2, max_score, index, index2, *start, num_gap,
      count;
   char ch, *sequence [2][2], *align, *gap [3];
   char_node *str, *last_char, *current_char;

   if (argc > 8)
   {
      printf ("Usage: mutation [codon_index] [codon_start] [region_start] [region_end] [match] [mismatch] [indel]\n");
      exit (-1);
   }
   if (argc <= 1)
      codon_index = CODON_INDEX;
   else
      codon_index = atoi (argv [1]);
   if (argc <= 2)
      codon_start = CODON_START;
   else
      codon_start = atoi (argv [2]);
   if (argc <= 3)
      region_start = REGION_START;
   else
      region_start = atoi (argv [3]);
   if (argc <= 4)
      region_end = REGION_END;
   else
      region_end = atoi (argv [4]);
   if (argc <= 5)
      match = MATCH;
   else
      match = atoi (argv [5]);
   if (argc <= 6)
      mismatch = MISMATCH;
   else
      mismatch = atoi (argv [6]);
   if (argc <= 7)
      indel = INDEL;
   else
      indel = atoi (argv [7]);
   len_str = 0;
   str = NULL;
   for (i = 0; i < 2; i++)
   {
      for (j = 0; j < 2; j++)
         sequence [i][j] = NULL;
   }
   found = 0;
   found2 = 0;
   do
   {
      ch = getchar ();
      if (found < 0)
      {
         if (ch == '\n')
            found++;
      }
      else if (found == 0)
      {
         if ((ch == '>') || (ch == '@'))
            found++;
      }
      else if (((found == 1) && (ch != '\n')) || islower (ch) || isupper (ch))
      {
         current_char = (char_node *) malloc (sizeof (char_node));
         if (errno == ENOMEM)
         {
            printf ("Error: Not enough memory for current_char\n");
            exit (-1);
         }
         current_char -> ch = ch;
         current_char -> next = NULL;
         if (str == NULL)
            str = current_char;
         else
            last_char -> next = current_char;
         last_char = current_char;
         len_str++;
      }
      else if ( (len_str > 0)
             && ((found == 1) || (ch == '>') || (ch == '+') || (ch == EOF)) )
      {
         if (sequence [found - 1][found2] != NULL)
            free (sequence [found - 1][found2]);
         sequence [found - 1][found2] = (char *)
            malloc ((len_str + 1) * sizeof (char));
         if (errno == ENOMEM)
         {
            printf ("Error: Not enough memory for sequence\n");
            exit (-1);
         }
         for (i = 0; i < len_str; i++)
         {
            current_char = str;
            str = str -> next;
            sequence [found - 1][found2][i] = current_char -> ch;
            free (current_char);
         }
         sequence [found - 1][found2][len_str] = '\0';
         if (found == 1)
            found++;
         else
         {
            if (ch == '>')
               found = 1;
            else if (ch == '+')
               found = -2;
            len_sequence [found2] = len_str;
            if (!found2)
            {
               score = (int **)
                  malloc ((len_sequence [0] / 3 + 1) * sizeof (int *));
               if (errno == ENOMEM)
               {
                  printf ("Error: Not enough memory for score\n");
                  exit (-1);
               }
               move = (int **)
                  malloc ((len_sequence [0] / 3 + 1) * sizeof (int *));
               if (errno == ENOMEM)
               {
                  printf ("Error: Not enough memory for move\n");
                  exit (-1);
               }
               move2 = (int **)
                  malloc ((len_sequence [0] / 3 + 1) * sizeof (int *));
               if (errno == ENOMEM)
               {
                  printf ("Error: Not enough memory for move2\n");
                  exit (-1);
               }
               align = (char *)
                  malloc (3 * (len_sequence [0] / 3) * sizeof (char));
               if (errno == ENOMEM)
               {
                  printf ("Error: Not enough memory for align\n");
                  exit (-1);
               }
               for (i = 0; i < 3 * (len_sequence [0] / 3); i++)
                  printf ("%c", sequence [1][0][i]);
               printf (" %s\n", sequence [0][0]);
               found2 = 1;
            }
            else
            {
               for (i = 0; i <= len_sequence [0] / 3; i++)
               {
                  score [i] = (int *)
                     malloc ((len_sequence [1] + 1) * sizeof (int));
                  if (errno == ENOMEM)
                  {
                     printf ("Error: Not enough memory for score\n");
                     exit (-1);
                  }
                  for (j = 0; j <= len_sequence [1]; j++)
                     score [0][j] = 0;
                  for (j = 0; j <= min (2, len_sequence [1]); j++)
                     score [i][j] = 0;
                  move [i] = (int *)
                     malloc ((len_sequence [1] + 1) * sizeof (int));
                  if (errno == ENOMEM)
                  {
                     printf ("Error: Not enough memory for move\n");
                     exit (-1);
                  }
                  for (j = 0; j <= len_sequence [1]; j++)
                     move [0][j] = 0;
                  for (j = 0; j <= min (2, len_sequence [1]); j++)
                     move [i][j] = 0;
                  move2 [i] = (int *)
                     malloc ((len_sequence [1] + 1) * sizeof (int));
                  if (errno == ENOMEM)
                  {
                     printf ("Error: Not enough memory for move2\n");
                     exit (-1);
                  }
                  for (j = 0; j <= len_sequence [1]; j++)
                     move2 [0][j] = 0;
                  for (j = 0; j <= min (2, len_sequence [1]); j++)
                     move2 [i][j] = 0;
               }
               for (i = 1; i <= len_sequence [0] / 3; i++)
               {
                  for (j = 3; j <= len_sequence [1]; j++)
                  {
                     score [i][j] = - INFINITY;
                     found3 = 0;
                     for (k = 0; k < 3; k++)
                     {
                        if (tolower (sequence [1][0][3 * (len_sequence [0] / 3
                               - i + 1) - k - 1]) != tolower (sequence [1][1]
                               [len_sequence [1] - j - k + 2]))
                        {
                           found3 = 1;
                           break;
                        }
                     }
                     if ( ( (!found3)
                         && (score [i - 1][j - 3] + match > score [i][j]) )
                       || ( found3
                         && (score [i - 1][j - 3] + mismatch > score [i][j]) ) )
                     {
                        if (!found3)
                           score [i][j] = score [i - 1][j - 3] + match;
                        else
                           score [i][j] = score [i - 1][j - 3] + mismatch;
                        move [i][j] = 1;
                        move2 [i][j] = 3;
                     }
                     for (k = 0; k < 3; k++)
                     {
                        if (score [i - 1][j - k] + indel > score [i][j])
                        {
                           score [i][j] = score [i - 1][j - k] + indel;
                           move [i][j] = 1;
                           move2 [i][j] = k;
                        }
                     }
                     for (k = 0; k < 3; k++)
                     {
                        if (score [i][j - k - 1] + indel > score [i][j])
                        {
                           score [i][j] = score [i][j - k - 1] + indel;
                           move [i][j] = 0;
                           move2 [i][j] = k + 1;
                        }
                     }
                  }
               }
               max_score = - INFINITY;
               for (i = 0; i <= len_sequence [1]; i++)
               {
                  if (score [len_sequence [0] / 3][i] > max_score)
                  {
                     max_score = score [len_sequence [0] / 3][i];
                     index = len_sequence [0] / 3;
                     index2 = i;
                  }
               }
               for (i = 0; i <= len_sequence [0] / 3; i++)
               {
                  for (j = max (0, len_sequence [1] - 2); j <= len_sequence [1];
                       j++)
                  {
                     if (score [i][j] > max_score)
                     {
                        max_score = score [i][j];
                        index = i;
                        index2 = j;
                     }
                  }
                  free (score [i]);
               }
/*               if (index2 < len_sequence [1])
               {
                  printf (" ");
                  for (i = len_sequence [1]; i > index2; i--)
                     printf ("%c",
                        tolower (sequence [1][1][len_sequence [1] - i]));
                  printf (")1");
               }*/
               for (i = 0; i < 3 * (len_sequence [0] / 3 - index); i++)
                  align [i] = ' ';
               start = (int *) malloc (len_sequence [1] * sizeof (int));
               if (errno == ENOMEM)
               {
                  printf ("Error: Not enough memory for start\n");
                  exit (-1);
               }
               for (i = 0; i < 3; i++)
               {
                  gap [i] = (char *) malloc (len_sequence [1] * sizeof (char));
                  if (errno == ENOMEM)
                  {
                     printf ("Error: Not enough memory for gap\n");
                     exit (-1);
                  }
               }
               num_gap = 0;
               while (move [index][index2] || move2 [index][index2])
               {
                  if (move [index][index2])
                  {
                     if (move2 [index][index2] == 0)
                     {
                        for (i = 0; i < 3; i++)
                           align [3 * (len_sequence [0] / 3 - index) + i] = '*';
                     }
                     else if (move2 [index][index2] == 1)
                     {
                        if ( (sequence [1][0][3 * (len_sequence [0] / 3
                                 - index)] == sequence [1][1][len_sequence [1]
                                 - index2])
                          || ( (sequence [1][0][3 * (len_sequence [0] / 3
                                   - index) + 1] != sequence [1][1][len_sequence
                                   [1] - index2])
                            && (sequence [1][0][3 * (len_sequence [0] / 3
                                   - index) + 2] != sequence [1][1][len_sequence
                                   [1] - index2]) ) )
                        {
                           align [3 * (len_sequence [0] / 3 - index)]
                              = sequence [1][1][len_sequence [1] - index2];
                           align [3 * (len_sequence [0] / 3 - index) + 1] = '*';
                           align [3 * (len_sequence [0] / 3 - index) + 2] = '*';
                        }
                        else
                        {
                           align [3 * (len_sequence [0] / 3 - index)] = '*';
                           if (sequence [1][0][3 * (len_sequence [0] / 3
                                  - index) + 1] == sequence [1][1][len_sequence
                                  [1] - index2])
                           {
                              align [3 * (len_sequence [0] / 3 - index) + 1]
                                 = sequence [1][1][len_sequence [1] - index2];
                              align [3 * (len_sequence [0] / 3 - index) + 2]
                                 = '*';
                           }
                           else
                           {
                              align [3 * (len_sequence [0] / 3 - index) + 1]
                                 = '*';
                              align [3 * (len_sequence [0] / 3 - index) + 2]
                                 = sequence [1][1][len_sequence [1] - index2];
                           }
                        }
                     }
                     else if (move2 [index][index2] == 2)
                     {
                        if ( ( ( (sequence [1][0][3 * (len_sequence [0] / 3
                                     - index)] == sequence [1][1][len_sequence
                                     [1] - index2])
                              || (sequence [1][0][3 * (len_sequence [0] / 3
                                     - index) + 1] != sequence [1][1]
                                     [len_sequence [1] - index2]) )
                            && ( (sequence [1][0][3 * (len_sequence [0] / 3
                                     - index) + 1] == sequence [1][1]
                                     [len_sequence [1] - index2 + 1])
                              || (sequence [1][0][3 * (len_sequence [0] / 3
                                     - index) + 2] != sequence [1][1]
                                     [len_sequence [1] - index2 + 1]) ) )
                          || ( (sequence [1][0][3 * (len_sequence [0] / 3
                                   - index) + 1] == sequence [1][1][len_sequence
                                   [1] - index2])
                            && (sequence [1][0][3 * (len_sequence [0] / 3
                                   - index) + 1] == sequence [1][1][len_sequence
                                   [1] - index2 + 1])
                            && (sequence [1][0][3 * (len_sequence [0] / 3
                                   - index) + 2] != sequence [1][1][len_sequence
                                   [1] - index2 + 1]) ) )
                        {
                           align [3 * (len_sequence [0] / 3 - index)]
                              = sequence [1][1][len_sequence [1] - index2];
                           align [3 * (len_sequence [0] / 3 - index) + 1]
                              = sequence [1][1][len_sequence [1] - index2 + 1];
                           align [3 * (len_sequence [0] / 3 - index) + 2] = '*';
                        }
                        else
                        {
                           if ( (sequence [1][0][3 * (len_sequence [0] / 3
                                    - index)] == sequence [1][1][len_sequence
                                    [1] - index2])
                             || (sequence [1][0][3 * (len_sequence [0] / 3
                                    - index) + 1] != sequence [1][1]
                                    [len_sequence [1] - index2]) )
                           {
                              align [3 * (len_sequence [0] / 3 - index)]
                                 = sequence [1][1][len_sequence [1] - index2];
                              align [3 * (len_sequence [0] / 3 - index) + 1]
                                 = '*';
                           }
                           else
                           {
                              align [3 * (len_sequence [0] / 3 - index)] = '*';
                              align [3 * (len_sequence [0] / 3 - index) + 1]
                                 = sequence [1][1][len_sequence [1] - index2];
                           }
                           align [3 * (len_sequence [0] / 3 - index) + 2]
                              = sequence [1][1][len_sequence [1] - index2 + 1];
                        }
                     }
                     else
                     {
                        for (i = 0; i < 3; i++)
                           align [3 * (len_sequence [0] / 3 - index) + i]
                              = sequence [1][1][len_sequence [1] - index2 + i];
                     }
                     index2 -= move2 [index][index2];
                     index--;
                  }
                  else
                  {
                     start [num_gap] = 3 * (len_sequence [0] / 3 - index);
                     for (i = 0; i < move2 [index][index2]; i++)
                        gap [i][num_gap]
                           = sequence [1][1][len_sequence [1] - index2 + i];
                     for (i = move2 [index][index2]; i < 3; i++)
                        gap [i][num_gap] = ' ';
                     num_gap++;
                     index2 -= move2 [index][index2];
                  }
               }
               for (i = 0; i <= len_sequence [0] / 3; i++)
               {
                  free (move [i]);
                  free (move2 [i]);
               }
/*               if (index2 > 0)
               {
                  printf (" %d(", 3 * (len_sequence [0] / 3));
                  for (i = index2; i > 0; i--)
                     printf ("%c",
                        tolower (sequence [1][1][len_sequence [1] - i]));
               }
               printf ("\n");*/
               for (i = 3 * (len_sequence [0] / 3 - index);
                    i < 3 * (len_sequence [0] / 3); i++)
                  align [i] = ' ';
               for (i = 0; i < len_sequence [0] / 3; i++)
               {
                  if ( (i < region_start - codon_start)
                    || (i > region_end - codon_start) )
                  {
                     for (j = 0; j < 3; j++)
                     {
                        if (tolower (sequence [1][0][3 * i + j])
                               == tolower (align [3 * i + j]))
                           printf (".");
                        else if (align [3 * i + j] == '*')
                           printf ("-");
                        else
                           printf ("%c", tolower (align [3 * i + j]));
                     }
                  }
                  else
                  {
                     index = 0;
                     for (j = 0; j < 3; j++)
                     {
                        if (nucleotide (tolower (sequence [1][0][3 * i + j]))
                               < 0)
                        {
                           index = -1;
                           break;
                        }
                        index = 4 * index + nucleotide (tolower (sequence [1][0]
                           [3 * i + j]));
                     }
                     index2 = 0;
                     for (j = 0; j < 3; j++)
                     {
                        if (nucleotide (tolower (align [3 * i + j])) < 0)
                        {
                           index2 = -1;
                           break;
                        }
                        index2 = 4 * index2 + nucleotide (tolower (align [3 * i
                           + j]));
                     }
                     for (j = 0; j < 3; j++)
                     {
                        if (index == index2)
                           printf (",");
                        else if ( (index >= 0) && (index2 >= 0)
                               && ( (amino_acid [index] != amino_acid [index2])
                                 || (j != 1) )
                               && ( (codon [index2] == 1)
                                 || (codon [index2] == codon_index) ) )
                           printf ("%c", toupper (align [3 * i + j]));
                        else
                           printf ("%c", tolower (align [3 * i + j]));
                     }
                  }
               }
               printf (" %d [", max_score);
               count = 0;
               for (i = max (0, region_start - codon_start);
                    i <= min (len_sequence [0] / 3 - 1, region_end
                       - codon_start); i++)
               {
                  index = 0;
                  for (j = 0; j < 3; j++)
                  {
                     if (nucleotide (tolower (sequence [1][0][3 * i + j])) < 0)
                     {
                        index = -1;
                        break;
                     }
                     index = 4 * index + nucleotide (tolower (sequence [1][0][3
                        * i + j]));
                  }
                  index2 = 0;
                  for (j = 0; j < 3; j++)
                  {
                     if (nucleotide (tolower (align [3 * i + j])) < 0)
                     {
                        index2 = -1;
                        break;
                     }
                     index2
                        = 4 * index2 + nucleotide (tolower (align [3 * i + j]));
                  }
                  if ( (index >= 0) && (index2 >= 0)
                    && (amino_acid [index] != amino_acid [index2])
/*                    && ( (codon [index2] == 1)
                      || (codon [index2] == codon_index) )*/ )
                  {
                     if (count > 0)
                        printf (",");
                     printf ("%c%d%c", toupper (amino_acid [index]),
                        codon_start + i, toupper (amino_acid [index2]));
                     count++;
                  }
                  else if ( (index >= 0) && (align [3 * i] == '*')
                         && (align [3 * i + 1] == '*')
                         && (align [3 * i + 2] == '*') )
                  {
                     if (count > 0)
                        printf (",");
                     printf ("%c%d-", toupper (amino_acid [index]),
                        codon_start + i);
                     count++;
                  }
               }
               printf ("] %d", count);
               for (i = 0; i < num_gap; i++)
               {
                  printf (" %d(", start [i]);
                  for (j = 0; j < 3; j++)
                  {
                     if (gap [j][i] != ' ')
                        printf ("%c", tolower (gap [j][i]));
                  }
                  printf (")%d", start [i] + 1);
               }
               free (start);
               for (i = 0; i < 3; i++)
                  free (gap [i]);
               printf (" %s\n", sequence [0][1]);
            }
         }
         len_str = 0;
      }
   }
   while (ch != EOF);
   for (i = 0; i < len_str; i++)
   {
      current_char = str;
      str = str -> next;
      free (current_char);
   }
   for (i = 0; i < 2; i++)
   {
      for (j = 0; j < 2; j++) 
      {
         if (sequence [i][j] != NULL)
            free (sequence [i][j]);
      }
   }
   if (found2)
   {
      free (score);
      free (move);
      free (move2);
      free (align);
   }
}
