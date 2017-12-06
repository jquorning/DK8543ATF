--
--
--

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Interfaces;

package Database is

   package US renames Ada.Strings.Unbounded;

   type Job_Id is new Interfaces.Integer_64;

   subtype Job_Ref is String (1 .. 3);

   type Job_Desc is
      record
         Ref   : String (1 .. 3);
         Id    : Job_Id;
         Title : US.Unbounded_String;
      end record;

   package Job_Vectors is new Ada.Containers.Vectors (Positive, Job_Desc);
   type Job_Set is
      record
         Vector  : Job_Vectors.Vector;
         Current : Job_Id;
      end record;

   type List_Id is new Interfaces.Integer_64;

   type List_Desc is
      record
         Ref  : String (1 .. 3);
         Id   : List_Id;
         Name : US.Unbounded_String;
         Desc : US.Unbounded_String;
      end record;

   package List_Vectors is new Ada.Containers.Vectors (Positive, List_Desc);
   type List_Set is
      record
         Vector     : List_Vectors.Vector;
         Current    : List_Id;
         Name_Width : Natural;
      end record;

   Jobs  : Job_Set;
   Lists : List_Set;

   procedure Open;

   procedure Get_Current (Job  : out Job_Id;
                          List : out List_Id);

   procedure Get_Jobs (Jobs :    out Job_Set;
                       List : in     List_Id := 0);

   procedure Get_Lists (Lists : out List_Set);

   procedure Put_Jobs (Jobs : in Job_Set);
   procedure Put_Lists (Lists : in List_Set);

   procedure Show_List (List : in List_Id);
   procedure Show_Job (Job : in Job_Id);

   procedure Create_Job (Id    : in Job_Id;
                         Title : in String;
                         List  : in List_Id);

   procedure Create_List (Name : in String);

   function Get_Job_Id return Job_Id;

   function Lookup_List (List : in String) return List_Id;
   function Lookup_Job  (Job  : in String) return Job_Id;

   procedure Transfer (Job     : in Job_Id;
                       To_List : in List_Id);
end Database;
