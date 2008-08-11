//===========================================================================
// Dhrystone-2.1 Benchmark for LPC
//
// Don't take this too serious :-)                               Sven C. Dack
//===========================================================================

#define Default_Number_Of_Runs 10000

#pragma strong_types

//===========================================================================
// Support functions to simulate memory allocation and other stuff..

mixed *alloc_array_50()    { return allocate(50); }

mixed *alloc_array_50_50() {
  mixed *arr;
  int i;

  for (i = 50, arr = allocate(50); i--; arr[i] = allocate(50));
  return arr;
}

mapping alloc_record() {
  return ([ "Ptr_Comp": 0, "Discr": 0, "var_1": ({ 0, 0, "" }) ]);
}

mapping copy_record(mapping rec) {
  mapping r;

  r = copy(rec);
  r["var_1"] = ({}) + rec["var_1"];
  return r;
}

int times() {
  int *ru;

  ru = rusage();
  return ru[0] + ru[1];
}

//===========================================================================
// The global variables.

#define Ident_1    0
#define Ident_2    1
#define Ident_3    2
#define Ident_4    3
#define Ident_5    4

#define Enum_Comp  0
#define Int_Comp   1
#define Str_Comp   2

mapping Ptr_Glob, Next_Ptr_Glob;
int     Int_Glob;
int     Bool_Glob;
int     Ch_1_Glob, Ch_2_Glob;
mixed  *Arr_1_Glob;
mixed  *Arr_2_Glob;
int     Begin_Time, End_Time, User_Time;
float   Milliseconds, Dhrystones_Per_Second;

//===========================================================================
// Prototypes all functions (including those defined later).

void Proc_1(mapping m1);
void Proc_2(int i1);
void Proc_3(mapping m1);
void Proc_4();
void Proc_5();
void Proc_6(int i1, int i2);
void Proc_7(int i1, int i2, int i3);
void Proc_8(mixed *a1, mixed *a2, int i1, int i2);
int  Func_1(int i1, int i2);
int  Func_2(string s1, string s2);
int  Func_3(int i1);

//===========================================================================

float main(int Number_Of_Runs, int silent) {
  int    Int_1_Loc, Int_2_Loc, Int_3_Loc;
  int    Ch_Index;
  int    Enum_Loc;
  string Str_1_Loc, Str_2_Loc;
  int    Run_Index;

  if (Number_Of_Runs <= 0)
    Number_Of_Runs = Default_Number_Of_Runs;

  Arr_1_Glob = alloc_array_50();
  Arr_2_Glob = alloc_array_50_50();
  Str_1_Loc  = "";
  Str_2_Loc  = "";
  Next_Ptr_Glob                = alloc_record();
  Ptr_Glob                     = alloc_record();
  Ptr_Glob["Ptr_Comp"]         = Next_Ptr_Glob;
  Ptr_Glob["Discr"]            = Ident_1;
  Ptr_Glob["var_1"][Enum_Comp] = Ident_3;
  Ptr_Glob["var_1"][Int_Comp]  = 40;
  Ptr_Glob["var_1"][Str_Comp]  = "DHRYSTONE PROGRAM, SOME STRING";
  Str_1_Loc        = "DHRYSTONE PROGRAM, 1'ST STRING";
  Arr_2_Glob[8][7] = 10;
  if  (!silent)
  {
      write("Dhrystone Benchmark, Version 2.1 (Language: LPC)\n");
      write("Execution starts, " + Number_Of_Runs + " runs through Dhrystone\n");
  }
  Begin_Time = times();
  for (Run_Index = 1; Run_Index <= Number_Of_Runs; ++Run_Index) {
    Proc_5();
    Proc_4();
    Int_1_Loc = 2;
    Int_2_Loc = 3;
    Str_2_Loc = "DHRYSTONE PROGRAM, 2'ND STRING";
    Enum_Loc  = Ident_2;
    Bool_Glob = !Func_2(Str_1_Loc, Str_2_Loc);
    while (Int_1_Loc < Int_2_Loc) {
      Int_3_Loc = 5 * Int_1_Loc - Int_2_Loc;
      Proc_7(Int_1_Loc, Int_2_Loc, &Int_3_Loc);
      Int_1_Loc += 1;
    }
    Proc_8(Arr_1_Glob, Arr_2_Glob, Int_1_Loc, Int_3_Loc);
    Proc_1(Ptr_Glob);
    for (Ch_Index = 'A'; Ch_Index <= Ch_2_Glob; ++Ch_Index) {
	  if (Enum_Loc == Func_1(Ch_Index, 'C')) {
	    Proc_6(Ident_1, &Enum_Loc);
	    Str_2_Loc = "DHRYSTONE PROGRAM, 3'RD STRING";
	    Int_2_Loc = Run_Index;
	    Int_Glob  = Run_Index;
	  }
    }
    Int_2_Loc = Int_2_Loc * Int_1_Loc;
    Int_1_Loc = Int_2_Loc / Int_3_Loc;
    Int_2_Loc = 7 * (Int_2_Loc - Int_3_Loc) - Int_1_Loc;
    Proc_2(&Int_1_Loc);
  }
  End_Time = times();
  if (!silent)
  {
      write("Execution ends.\n");
      write("Final values of the variables used in the benchmark:\n\n");
      printf("Int_Glob:            %d\n", Int_Glob);
      printf("        should be:   %d\n", 5);
      printf("Bool_Glob:           %d\n", Bool_Glob);
      printf("        should be:   %d\n", 1);
      printf("Ch_1_Glob:           %c\n", Ch_1_Glob);
      printf("        should be:   %c\n", 'A');
      printf("Ch_2_Glob:           %c\n", Ch_2_Glob);
      printf("        should be:   %c\n", 'B');
      printf("Arr_1_Glob[8]:       %d\n", Arr_1_Glob[8]);
      printf("        should be:   %d\n", 7);
      printf("Arr_2_Glob[8][7]:    %d\n", Arr_2_Glob[8][7]);
      printf("        should be:   Number_Of_Runs + 10\n");
#if 0
      printf("Ptr_Glob->\n");
      printf("  Ptr_Comp:          %O\n", Ptr_Glob["Ptr_Comp"]);
      printf("        should be:   (implementation-dependent)\n");
#endif
      printf("  Discr:             %d\n", Ptr_Glob["Discr"]);
      printf("        should be:   %d\n", 0);
      printf("  Enum_Comp:         %d\n", Ptr_Glob["var_1"][Enum_Comp]);
      printf("        should be:   %d\n", 2);
      printf("  Int_Comp:          %d\n", Ptr_Glob["var_1"][Int_Comp]);
      printf("        should be:   %d\n", 17);
      printf("  Str_Comp:          %s\n", Ptr_Glob["var_1"][Str_Comp]);
      printf("        should be:   DHRYSTONE PROGRAM, SOME STRING\n");
#if 0
      printf("Next_Ptr_Glob->\n");
      printf("  Ptr_Comp:          %O\n", Next_Ptr_Glob["Ptr_Comp"]);
      printf("        should be:   (implementation-dependent), same as above\n");
#endif
      printf("  Discr:             %d\n", Next_Ptr_Glob["Discr"]);
      printf("        should be:   %d\n", 0);
      printf("  Enum_Comp:         %d\n", Next_Ptr_Glob["var_1"][Enum_Comp]);
      printf("        should be:   %d\n", 1);
      printf("  Int_Comp:          %d\n", Next_Ptr_Glob["var_1"][Int_Comp]);
      printf("        should be:   %d\n", 18);
      printf("  Str_Comp:          %s\n", Next_Ptr_Glob["var_1"][Str_Comp]);
      printf("        should be:   DHRYSTONE PROGRAM, SOME STRING\n");
      printf("Int_1_Loc:           %d\n", Int_1_Loc);
      printf("        should be:   %d\n", 5);
      printf("Int_2_Loc:           %d\n", Int_2_Loc);
      printf("        should be:   %d\n", 13);
      printf("Int_3_Loc:           %d\n", Int_3_Loc);
      printf("        should be:   %d\n", 7);
      printf("Enum_Loc:            %d\n", Enum_Loc);
      printf("        should be:   %d\n", 1);
      printf("Str_1_Loc:           %s\n", Str_1_Loc);
      printf("        should be:   DHRYSTONE PROGRAM, 1'ST STRING\n");
      printf("Str_2_Loc:           %s\n", Str_2_Loc);
      printf("        should be:   DHRYSTONE PROGRAM, 2'ND STRING\n");
      printf("\n");
  }
  User_Time = End_Time - Begin_Time;
  if (User_Time <= 0) {
    if (!silent) {
        printf("Your rusage() doesn't seem to work: User_Time is %d.\n", User_Time);
        write("\n");
    }
  } else if (User_Time < (2*100)) {
    if (!silent) {
        write("Measured time too small to obtain meaningful results\n");
        write("Please increase number of runs\n");
        write("\n");
    }
  } else {
    Milliseconds = to_float(User_Time) / Number_Of_Runs;
    Dhrystones_Per_Second = 1000.0 * Number_Of_Runs / User_Time;
    if (!silent) {
        printf ("Milliseconds for one run through Dhrystone: ");
        printf ("%6.1f \n", Milliseconds);
        printf ("Dhrystones per Second:                      ");
        printf ("%6.1f \n", Dhrystones_Per_Second);
    }
  }

  return Dhrystones_Per_Second;
}

void Proc_1(mapping Ptr_Val_Par) {
  mapping Next_Record;

  Next_Record                    = Ptr_Val_Par["Ptr_Comp"];
  Ptr_Val_Par["Ptr_Comp"]        = copy_record(Ptr_Glob);
  Ptr_Val_Par["var_1"][Int_Comp] = 5;
  Next_Record["var_1"][Str_Comp] = Ptr_Val_Par["var_1"][Str_Comp];
  Next_Record["var_1"][Int_Comp] = Ptr_Val_Par["var_1"][Int_Comp];
  Next_Record["Ptr_Comp"]        = Ptr_Val_Par["Ptr_Comp"];
  Proc_3(&(Next_Record["Ptr_Comp"]));
  if (Next_Record["Discr"] == Ident_1) {
    Next_Record["var_1"][Int_Comp] = 6;
    Proc_6(  Ptr_Val_Par["var_1"][Enum_Comp],
	   &(Next_Record["var_1"][Enum_Comp]));
    Next_Record["Ptr_Comp"] = Ptr_Glob["Ptr_Comp"];
    Proc_7(  Next_Record["var_1"][Int_Comp], 10,
	   &(Next_Record["var_1"][Int_Comp]));
  } else
    Ptr_Val_Par = copy_record(Ptr_Val_Par["Ptr_Comp"]);
}

void Proc_2(int Int_Par_Ref) {
  int Int_Loc, Enum_Loc;

  Int_Loc = Int_Par_Ref + 10;
  do
    if (Ch_1_Glob == 'A') {
      Int_Loc -= 1;
      Int_Par_Ref = Int_Loc - Int_Glob;
      Enum_Loc = Ident_1;
    } while (Enum_Loc != Ident_1);
}

void Proc_3(mapping Ptr_Ref_Par) {
  if (Ptr_Glob != 0)
    Ptr_Ref_Par = Ptr_Glob["Ptr_Comp"];
  Proc_7(10, Int_Glob, &(Ptr_Glob["var_1"][Int_Comp]));
}

void Proc_4() {
  int Bool_Loc;
  Bool_Loc = Ch_1_Glob == 'A';
  Bool_Glob = Bool_Loc | Bool_Glob;
  Ch_2_Glob = 'B';
}

void Proc_5() {
  Ch_1_Glob = 'A';
  Bool_Glob = 0 ;
}

void Proc_6(int Enum_Val_Par, int Enum_Ref_Par) {
  Enum_Ref_Par = Enum_Val_Par;
  if (!Func_3(Enum_Val_Par))
    Enum_Ref_Par = Ident_4;
  switch (Enum_Val_Par) {
  case Ident_1:
    Enum_Ref_Par = Ident_1;
    break;
  case Ident_2:
    if (Int_Glob > 100)
      Enum_Ref_Par = Ident_1;
    else
      Enum_Ref_Par = Ident_4;
    break;
  case Ident_3:
    Enum_Ref_Par = Ident_2;
    break;
  case Ident_4:
    break;
  case Ident_5:
    Enum_Ref_Par = Ident_3;
    break;
  }
}

void Proc_7(int Int_1_Par_Val, int Int_2_Par_Val, int Int_Par_Ref) {
  int Int_Loc;

  Int_Loc = Int_1_Par_Val + 2;
  Int_Par_Ref = Int_2_Par_Val + Int_Loc;
}

void Proc_8 (mixed *Arr_1_Par_Ref, mixed *Arr_2_Par_Ref,
        int Int_1_Par_Val, int Int_2_Par_Val) {
  int Int_Index, Int_Loc;

  Int_Loc = Int_1_Par_Val + 5;
  Arr_1_Par_Ref[Int_Loc]      = Int_2_Par_Val;
  Arr_1_Par_Ref[Int_Loc + 1]  = Arr_1_Par_Ref[Int_Loc];
  Arr_1_Par_Ref[Int_Loc + 30] = Int_Loc;
  for (Int_Index = Int_Loc; Int_Index <= Int_Loc + 1; ++Int_Index)
    Arr_2_Par_Ref[Int_Loc][Int_Index]  = Int_Loc;
  Arr_2_Par_Ref[Int_Loc][Int_Loc - 1] += 1;
  Arr_2_Par_Ref[Int_Loc + 20][Int_Loc] = Arr_1_Par_Ref[Int_Loc];
  Int_Glob = 5;
}

int Func_1(int Ch_1_Par_Val, int Ch_2_Par_Val) {
  int Ch_1_Loc, Ch_2_Loc;

  Ch_1_Loc = Ch_1_Par_Val;
  Ch_2_Loc = Ch_1_Loc;
  if (Ch_2_Loc != Ch_2_Par_Val)
    return Ident_1;
  else {
    Ch_1_Glob = Ch_1_Loc;
    return Ident_2;
  }
}

int Func_2(string Str_1_Par_Ref, string Str_2_Par_Ref) {
  int Int_Loc, Ch_Loc;

  Int_Loc = 2;
  while (Int_Loc <= 2)
    if (Func_1(Str_1_Par_Ref[Int_Loc], Str_2_Par_Ref[Int_Loc + 1]) == Ident_1) {
      Ch_Loc = 'A';
      Int_Loc += 1;
    }
  if (Ch_Loc >= 'W' && Ch_Loc < 'Z')
    Int_Loc = 7;
  if (Ch_Loc == 'R')
    return 1;
  else {
    if (Str_1_Par_Ref > Str_2_Par_Ref) {
      Int_Loc += 7;
      Int_Glob = Int_Loc;
      return 1;
    } else
      return 0;
  }
}

int Func_3(int Enum_Par_Val) {
  int Enum_Loc;

  Enum_Loc = Enum_Par_Val;
  if (Enum_Loc == Ident_3)
    return 1;
  else
    return 0;
}

