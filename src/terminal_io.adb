--
--
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Symbols;
with Commands;

package body Terminal_IO is


   procedure Put_Jobs (Jobs : in Database.Job_Set) is
      use Ada.Text_IO;
      use Database;
      Current_Job : constant Database.Job_Id := Database.Get_Current_Job;
   begin
      for Job of Jobs.Vector loop
         if Job.Id = Current_Job then
            Put (Symbols.White_Right_Pointing_Index);
         else
            Put (" ");
         end if;
         Put (" ");

         Put (Symbols.White_Star);
         Put (" ");
         Put (Job.Ref);

         Put (" ");
         Put (US.To_String (Job.Title));
         New_Line;
      end loop;
   end Put_Jobs;


--     procedure Put_Lists (Lists : in Database.List_Set) is
--        use Ada.Text_IO;
--        use Database;
--     begin
--        for List of Lists.Vector loop
--           declare
--              Name_Image : String (1 .. Lists.Name_Width) := (others => ' ');
--              Name       : constant String := US.To_String (List.Name);
--           begin
--              if List.Id = Lists.Current then
--                 Put (Symbols.White_Right_Pointing_Index);
--              else
--                 Put (" ");
--              end if;
--              Put (" ");

--              Put (Symbols.White_Star);
--              Put (" ");
--              Put (List.Ref);

--              Put (" ");
--              Name_Image (1 .. Name'Last) := Name;
--              Put (Name_Image);
--              Put (" ");
--              Put (US.To_String (List.Desc));
--           end;
--           New_Line;
--        end loop;
--     end Put_Lists;


   procedure Show_Job (Job : in Database.Job_Id) is
      use Ada.Text_IO, Ada.Strings.Unbounded;

      Info   : constant Database.Job_Info  := Database.Get_Job_Info (Job);
      Events : constant Database.Event_Lists.Vector
        := Database.Get_Job_Events (Job);
   begin
      Put_Line (To_String (Info.Title) & " (" & Job'Img & ")");
      Put ("Parent (" & Info.Parent'Img & ")");
      Put ("Owner: "  & To_String (Info.Owner));
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
