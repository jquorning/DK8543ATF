--
--
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Symbols;

package body Terminal_IO is


   procedure Put_Jobs (Jobs : in Database.Job_Set) is
      use Ada.Text_IO;
      use Database;
   begin
      for Job of Jobs.Vector loop
         Put (Symbols.White_Star);
         Put ("  ");
         Put (Job.Ref);
         Put ((if Job.Id = Jobs.Current then "*" else " "));
         Put ("  ");
         Put (US.To_String (Job.Title));
         New_Line;
      end loop;
   end Put_Jobs;


   procedure Put_Lists (Lists : in Database.List_Set) is
      use Ada.Text_IO;
      use Database;
   begin
      for List of Lists.Vector loop
         declare
            Name_Image : String (1 .. Lists.Name_Width) := (others => ' ');
            Name       : constant String := US.To_String (List.Name);
         begin
            Put (Symbols.White_Star);
            Put ("  ");
            Put (List.Ref);
            Put ((if List.Id = Lists.Current then "*" else " "));
            Put ("  ");
            Name_Image (1 .. Name'Last) := Name;
            Put (Name_Image);
            Put ("  ");
            Put (US.To_String (List.Desc));
         end;
         New_Line;
      end loop;
   end Put_Lists;


   procedure Show_Job (Job : in Database.Job_Id) is
      use Ada.Text_IO, Ada.Strings.Unbounded;

      Info   : constant Database.Job_Info  := Database.Get_Job_Info (Job);
      Events : constant Database.Event_Lists.Vector
        := Database.Get_Job_Events (Job);
   begin
      Put_Line (To_String (Info.Title) & " (" & Job'Img & ")");
      Put ("List (" & Info.List'Img & ")");
      Put ("Owner: " & To_String (Info.Owner));
      New_Line;

      for Event of Events loop
         Put (To_String (Event.Stamp));
         Put ("    ");
         Put (To_String (Event.Kind));
         New_Line;
      end loop;
   end Show_Job;


end Terminal_IO;
