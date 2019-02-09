--
--  Types
--  Common types

--  with Iterfaces;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package Types is

   type Job_Id is new Long_Integer; -- Interfaces.Integer_64;

   package US renames Ada.Strings.Unbounded;

   type Job_Desc is
      record
         Id    : Types.Job_Id;
         Title : US.Unbounded_String;
      end record;

   type Job_Index is new Positive;
   package Job_Sets is
      new Ada.Containers.Vectors (Job_Index, Job_Desc);

   type Job_Info is
      record
         Title  : US.Unbounded_String;
         Parent : Types.Job_Id;
         Owner  : US.Unbounded_String;
      end record;

end Types;
