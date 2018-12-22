--
--  CSV_IO body
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Database;

package body CSV_IO is

   procedure Export (File_Name : String) is
      use Ada.Text_IO;

      Export_File : File_Type;

      procedure Put_Row (Col_1, Col_2, Col_3, Col_4 : String);
      procedure Put_Row (Col_1, Col_2, Col_3, Col_4 : String) is
      begin
         Put_Line (Export_File,
                   Col_1 & ";" & Col_2 & ";" &
                     Col_3 & ";" & Col_4 & ";");
      end Put_Row;

      Lists : Database.List_Set;
   begin
      Create (Export_File, Out_File, File_Name);

      Database.Get_Lists (Lists);
      for List of Lists.Vector loop

         Put_Row (List.Ref,
                  Ada.Strings.Unbounded.To_String (List.Name),
                  List.Id'Img,
                  Ada.Strings.Unbounded.To_String (List.Desc));
         declare
            Jobs : Database.Job_Set;
         begin
            Database.Get_Jobs (Jobs, List => List.Id);

            for Job of Jobs.Vector loop
               Put_Row (Col_1 => Job.Ref,
                        Col_2 => Ada.Strings.Unbounded.To_String (Job.Title),
                        Col_3 => Job.Id'Img,
                        Col_4 => "");
               declare
                  Events : constant Database.Event_Lists.Vector :=
                    Database.Get_Job_Events (Job.Id);
               begin
                  for Event of Events loop
                     Put_Row ("",
                              Ada.Strings.Unbounded.To_String (Event.Stamp),
                              Ada.Strings.Unbounded.To_String (Event.Kind),
                              "");
                  end loop;
               end;
            end loop;
         end;
         Put_Row ("", "", "", "");
      end loop;

      Close (Export_File);
   end Export;

end CSV_IO;
