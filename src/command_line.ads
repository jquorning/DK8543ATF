--
--
--

with Ada.Command_Line;

with Setup;

package Command_Line is


   Terminate_Program : exception;
   --  Raised when program should terminate.


   procedure Parse (Config : in out Setup.Configuration);
   --  Parse command line and change Config accordingly. Print out
   --  program version or help text and raise Terminate_Program.


   procedure Put_Usage;
   --  Show program usage text to screen.


   procedure Put_Version;
   --  Show program name and version to screen.


   subtype Exit_Status is Ada.Command_Line.Exit_Status;

   Success : Exit_Status renames Ada.Command_Line.Success;
   Failure : Exit_Status renames Ada.Command_Line.Failure;

   procedure Set_Exit_Status (Status : Exit_Status) renames
     Ada.Command_Line.Set_Exit_Status;
   --  Set program exit status.


end Command_Line;
