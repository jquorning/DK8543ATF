--
--
--

with Ada.Strings.Unbounded;

with Database;

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
      (-"view",  -"Show current list"),
      (-"lists", -"Show all list"),
      (-"set list LIST", -"Set LIST to current"),
      (-"set job JOB",   -"Set JOB to current"),
      (-"show list",
        -"Show all jobs in current list"),
      (-"show job",      -"Show jobs details"),
      (-"add job TITLE", -"Add job to current list"),
      (-"add list NAME", -"Add list to database"),
      (-"split serial JOB COUNT",
       -"Split job into count serial jobs"),
      (-"split parallel JOB COUNT ",
       -"Split job into count parallel jobs"),
      (-"move LIST",
       -"Move current job to other list"),
      (-"trans LIST",
       -"Transfer current job to other list"),
      (-"event KIND", -"Add event to current job"));


   procedure Create_Job (Job   : in Database.Job_Id;
                         Title : in String;
                         List  : in Database.List_Id);

   procedure Show_List (List : in Database.List_Id);

end Commands;
