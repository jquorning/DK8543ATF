--
--
--

with Ada.Text_IO;

package body Terminal_IO is


   procedure Put_Jobs (Jobs : in Database.Job_Set) is
      use Ada.Text_IO;
      use Database;
   begin
      for Job of Jobs.Vector loop
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


end Terminal_IO;
