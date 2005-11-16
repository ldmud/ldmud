#if 0
--------------------------------------------------------------------------
Date: Thu, 13 Dec 2001 00:34:12 -0700
From: Acius <helpsfamily@attbi.com>

A while back we were talking about eventually putting path-finding in
the driver, but I don't know if anyone came up with anything really
specific (I might add that I've been off the mailing list for a little
while now, thanks to @Home's problems). I wanted path-finding for my
MUD, and since it sounded like it might be a while until Lars has enough
time to play with it, I've implemented A-star in LP/C. The code is
attached to this message.

I have a couple reasons for posting the code: Firstly, anyone who might
want to use it can try it, and if they post their modifications and
bugfixes, I get better code for free :-). Also, it serves as an
interface and behavior suggestion for future code to be put into the
driver. I wrote it with this idea in mind, and I have attempted to make
the code a very general way to do 2d path-finding. The code could be
placed in a simul_efun for now, and simply commented out or removed when
and if driver path-finding code comes on line.

The parameters are:

* A string, giving the map on which you're doing the search
* The x, y of the starting location
* The x, y of the goal location
* A mapping of ([ character: cost ]) for all of the characters used in
the map string. All characters not included in the mapping are assumed
to be walls. A cost of 0 might not get the best path if you use it,
since Manhattan distance is used to guide the search and it will
overestimate (go read a good article on A-star).

Here's a quick example of how to use it:

.*.#.
.#...
...

I want to get from the upper left corner (0, 0) to the upper right
corner (4, 0), and I need to know the path to get there. The #'s are
walls, and may not be crossed. The '*' may be crossed, but it is a
"poisonous" tile and should not be crossed if there is an alternative.
Note that the grid doesn't have to be square; the function will append
each line with "\27"'s to make it square.

Here's how to make a function call to get this path:

string path;
path =
    find_path(".*.#.\n.#...\n...", 0, 0, 4, 0, ([ '.': 1, '*': 100 ]));
if( path )
    write("You should go "+path+"\n");
else
    write("No path could be found.\n");

On my MUD, the call to find_path returns the string "sseeneen". If you
lower the cost of '*' to 5 (just change the 100 to a 5), then it returns
the string "eeseen".

If there are any other questions, please email me; this message is
getting long enough already. I particularly want to hear from you if you
get it working (or can't!) and if you find bugs or ways to make it more
efficient (it is not very efficient). I tried not to put in anything
MUDlib-specific, but sometimes I forget.

-- Acius
--------------------------------------------------------------------------
#endif

string find_path( string map, int startx, int starty, int goalx, int goaly, mapping costs ) {
   // split_map will be an array of equal-length
   //strings (will pad spaces to assure this)
   string *split_map;

   // i is for generic loops, map_width is width
   // of split_map, goal is the destination point
   // (All coordinates are stored as x + y *
   // width). currpos is where the algorithm is
   // looking, currcost is cost at currpos, newx,
   // newy, newpos, and newcost are temps for
   // figuring out where to look next.
   int i, map_width, start, goal, currpos, currcost, newx, newy, newpos, newcost;
   int ineighbor, iparent, iheap;

   // closed is mapping from (int)coordinate:
   // (int) cost & direction. Note that costs
   // > 2^28 will cause undetected overflow
   // problems -- if you keep costs reasonable,
   // you shouldn't have a problem.
   mapping closed;

   // The open list, stored as a heap.
   // Even-numbered elements hold coordinate information,
   // odd-numbered ones give the cost for the previous
   // even numbered coordinate.
   int *open;
   string path;

   mixed tmp;

   // Process the map into an easier-to-use format.
   split_map = explode( map, "\n" );
   map_width = 0;

   // Find the length of the longest line in the "map"
   for( i = sizeof(split_map); i-- ; )
      if( map_width < strlen(split_map[i]) ) map_width = strlen(split_map[i]);

   // Make all the lines that length by padding with escape characters.
   // (Note: I use escapes because they are an unlikely character to be
   // chosen for walking over, and unused characters are 'walls')
   for( i = sizeof(split_map); i-- ; )
      split_map[i] += sprintf( "%" + (map_width-strlen(split_map[i])) + "'\27's", "" );

   // Sanity check
   if( goalx < 0  || goalx >= map_width || goaly < 0 || goaly >= sizeof(split_map) )
      return 0;
   // Setup initial state.
   start = startx + starty * map_width;
   goal = goalx + goaly * map_width;
   open = ({ 0, start });
   closed = ([ start:0 ]);

   while( sizeof(open) && !member(closed, goal) ) {
      currcost = open[0];
      currpos = open[1];
      // Check if done.
      if( currpos == goal ) {
         closed[currpos] = currcost;
         break;
      }

      // Pop the top cell off the heap.
      open[0] = open[<2];
      open[1] = open[<1];
      open = open[0..<3];
      iparent = 0;
      iheap = 2;
      while( iheap < sizeof(open) ) {
         if( open[iparent] > open[iheap] && (iheap + 2 >= sizeof(open) || open[iheap] < open[iheap + 2]) ) {
            tmp = open[iparent..iparent+1];
            open[iparent..iparent + 1] = open[iheap..iheap + 1];
            open[iheap..iheap + 1] = tmp;
         }
         else if( iheap + 2 < sizeof(open) && open[iparent] > open[iheap + 2] ) {
            tmp = open[iparent..iparent+1];
            open[iparent..iparent + 1] = open[iheap + 2..iheap + 3];
            open[iheap + 2..iheap + 3] = tmp;
         }
         else
            break;
         iparent = iheap;
         iheap = (iparent + 1) << 1;
      }

      // Add the neighbors of the popped cell to the heap
      // 0 = east, 1 = north, 2 = west, 3 = south
      for( ineighbor = 0; ineighbor < 4; ineighbor++ ) {
         newx = (currpos % map_width) + ({ 1, 0, -1, 0 })[ineighbor];
         newy = (currpos / map_width) + ({ 0, -1, 0, 1 })[ineighbor];
         newcost = ({ 0x0, 0x1, 0x2, 0x3 })[ineighbor];
         // Out of bounds, ignore it.
         if( newx < 0 || newx >= map_width || newy < 0 || newy >= sizeof(split_map) )
            continue;
         // Solid wall, ignore it.
         if( !member(costs, split_map[newy][newx]) ) continue;
         newpos = newx + newy * map_width;
         // Already checked, ignore it.
         if( member(closed, newpos) ) continue;
         // Ok, it's legit. Add it to the heap:
         newcost |= (currcost & 0xFFFC) + (costs[split_map[newy][newx]] << 2);
         newcost -= (abs(currpos % map_width) + abs(currpos / map_width)) << 2;
         newcost += (abs(goalx - newx) + abs(goaly - newy)) << 2;
         // Mark this cell as 'considered'
         closed[newpos] = newcost;
         iheap = sizeof(open);
         open += ({ newcost, newpos });
         if( iheap > 0 )
         while( open[iparent = ((iheap >> 1) - 1) & -2] > open[iheap] ) {
            tmp = open[iparent..iparent+1];
            open[iparent..iparent+1] = open[iheap..iheap+1];
            open[iheap..iheap+1] = tmp;
         }
      }
   }
   i = goal;
   if( !member(closed, i) ) return 0;
   path = "";
   while( i != start ) {
      path = ({ "e", "n", "w", "s" })[closed[i]&0x3] + path;
      i += ({ -1, map_width, 1, -map_width })[closed[i]&0x3];
   }
   return path;
}

