#include <stdio.h>
#include <string.h>

main ()
{
   int i, count, count2, len_sequence, len_align, found;
   char buffer [1000000], sequence [1000000], align [1000000],
      mutation [1000000];

   count = 0;
   count2 = 0;
   gets (buffer);
   sscanf (buffer, "%s", sequence);
   len_sequence = strlen (sequence);
   while (gets (buffer) != NULL)
   {
      if ((buffer [0] != ' ') && (strchr (buffer, '(') == NULL))
      {
         sscanf (buffer, "%s %*s %s", align, mutation);
         len_align = strlen (align);
         if (len_align >= len_sequence)
         {
            found = 0;
            for (i = 0; i < len_align; i++)
            {
               if ( (align [i] == '-')
                 || ( (islower (align [i]))
                   && ( ( (i % 3 == 0)
                       && ( (!isupper (align [i + 1]))
                         || (!isupper (align [i + 2])) ) )
                     || ( (i % 3 == 1)
                       && ( (!isupper (align [i - 1]))
                         || (!isupper (align [i + 1])) ) )
                     || ( (i % 3 == 2)
                       && ( (!isupper (align [i - 2]))
                         || (!isupper (align [i - 1])) ) ) ) )
                 || ( (align [i] == '*')
                   && ( ( (i % 3 == 0)
                       && ((align [i + 1] != '*') || (align [i + 2] != '*')) )
                     || ( (i % 3 == 1)
                       && ((align [i - 1] != '*') || (align [i + 1] != '*')) )
                     || ( (i % 3 == 2)
                       && ( (align [i - 2] != '*')
                         || (align [i - 1] != '*') ) ) ) ) )
               {
                  found = 1;
                  break;
               }
            }
            if (!found)
            {
               buffer [0] = '\0';
               sscanf (&mutation [1], "%[^],]", buffer);
               if (mutation [strlen (buffer) + 1] == ']')
                  printf ("%s\n", buffer);
               printf ("%s\n", mutation);
               count++;
            }
         }
      }
      count2++;
   }
   for (i = 0; i < count; i++)
      printf ("%d\n", count2);
}
