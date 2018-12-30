--
--
--

with Ada.Strings.Unbounded;

with Database.Jobs;

package Commands is

   use Ada.Strings.Unbounded;
   function "-" (Source : String) return Unbounded_String
     renames To_Unbounded_String;

   type Help_Line is
      record
         Command : Unbounded_String;
         Comment : Unbounded_String;
      end record;

   Help_Lines : constant array (Positive range <>) of Help_Line :=
     ((-"help",  -"Print this help text"),
      (-"quit",  -"Quit program"),
      (-"ls",    -"Show current list"),
      (-"view",  -"Show current list"),
      (-"top",   -"Show top level jobs"),
      (-"cd",    -"Show top level jobs"),
      (-"set JOB",       -"Set JOB to current"),
      (-"show",          -"Show jobs details"),
      (-"cat",           -"Show jobs details"),
      (-"add job TITLE", -"Add job to current list"),
      (-"add list NAME", -"Add list to database"),
      (-"split serial JOB COUNT",
       -"Split job into count serial jobs"),
      (-"split parallel JOB COUNT ",
       -"Split job into count parallel jobs"),
      (-"text TEXT",
       -"Add free text to current job"),
      (-"move LIST",
       -"Move current job to other list"),
      (-"trans LIST",
       -"Transfer current job to other list"),
      (-"event KIND", -"Add event to current job"),
      (-"export", -"Export jobs to csv file (todo.csv)"));


   procedure Create_Job (Job    : in Database.Jobs.Job_Id;
                         Title  : in String;
                         Parent : in Database.Jobs.Job_Id);

end Commands;
