--
--
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Symbols;
with Commands;
with Database.Events;

package body Terminal_IO is


   procedure Put_Jobs (Jobs : in Database.Job_Set) is
      use Ada.Text_IO;
      use Database;

      Current_Job : constant Job_Id := Get_Current_Job;
   begin

      if Jobs.Vector.Is_Empty then
         Put_Line ("<< No Jobs >>");
         return;
      end if;

      for Job of Jobs.Vector loop
         if Job.Id = Current_Job then
            Put (Symbols.White_Right_Pointing_Index);
         else
            Put (" ");
         end if;
         Put (" ");

         if Events.Is_Done (Job.Id) then
            Put (Symbols.Black_Star);
         else
            Put (Symbols.White_Star);
         end if;

         Put (" ");
         Put (Job.Ref);

         Put (" ");
         Put (US.To_String (Job.Title));
         New_Line;
      end loop;
   end Put_Jobs;


   procedure Show_Job (Job : in Database.Job_Id) is
      use Ada.Text_IO, Ada.Strings.Unbounded;

      Info   : constant Database.Job_Info  := Database.Get_Job_Info (Job);
      Events : constant Database.Events.Event_Lists.Vector
        := Database.Events.Get_Job_Events (Job);
      Done       : constant Boolean := Database.Events.Is_Done (Job);
      Done_Image : constant String  := Boolean'Image (Done);
   begin
      Put_Line (To_String (Info.Title) & " (Id" & Job'Img & ")");
      Put_Line ("Parent (Id" & Info.Parent'Img & ")");
      Put_Line ("Owner  (" & To_String (Info.Owner) & ")");
      Put_Line ("Status DONE: " & Done_Image);
      New_Line;

      for Event of Events loop
         Put (To_String (Event.Stamp));
         Put ("    ");
         Put (To_String (Event.Kind));
         New_Line;
      end loop;
   end Show_Job;


   procedure Put_Help is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
   begin
      for Line of Commands.Help_Lines loop
         Set_Col (1);
         Put (To_String (Line.Command));
         Set_Col (33);
         Put_Line (To_String (Line.Comment));
      end loop;
   end Put_Help;


   procedure Put_Error (Text : in String) is
   begin
      Ada.Text_IO.Put_Line (Text);
   end Put_Error;


   procedure Put_Banner is
      use Ada.Text_IO;
   begin
      Put_Line ("TODO List program");
      Put_Line ("=================");
      Put_Line ("type help to show help text");
   end Put_Banner;


end Terminal_IO;
