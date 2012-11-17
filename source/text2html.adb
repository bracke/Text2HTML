with Utility;                 use Utility;
--with Reporter;
with Ada.Text_IO;
with FileInternal;            use FileInternal;
with FileExternal;            use FileExternal;
with Ada.Strings;             use Ada.Strings;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;

procedure Text2HTML is

   procedure Main is

--      Version      : String := "1.05";
--      VString      : String := "<hr><address><small>Created by <a href=" &
--                       & '"' & "http://www.arcsite.de/hp/bracke/programming/mtools.html" &
--                       & '"' & ">Text2HTML</a> v" & Version & "</small></address>";
      Source       : String := Argument(1);
      Target       : String := Argument(2);
      TFile        : FileHandle_Type(new UString'(U(Target)),Write);
      SFile        : FileHandle_Type(new UString'(U(Source)),Read);
      Byte         : integer;
--      Previous     : integer := -1;
   begin
--      pragma Debug(Reporter.Text("VString: _" & VString & "_"));
      
      -- Header
      if Argument_Count > 2 then
         declare
            Header : FileHandle_Type(new UString'(U(Argument(3))),Read);
         begin

            while not isEOF(Header) loop
               Byte := FileInternal.Get_Byte(Header);
               FileInternal.Put_Byte(TFile,Byte);
            end loop;
         end;
         if Argument_Count >= 4 then
            declare
               BodyTag : FileHandle_Type(new UString'(U(Argument(4))),Read);
            begin
               while not isEOF(BodyTag) loop
                  Byte := Get_Byte(BodyTag);
                  Put_Byte(TFile,Byte);
               end loop;
            end;
         else
            FileInternal.Put_String(TFile,"<BODY>");
         end if;
      else
         Put_String(TFile,"<!DOCTYPE HTML PUBLIC " & '"' & "-//W3C//DTD HTML 4.01 Transitional//EN" & '"' & '>');
         Put_String(TFile,"<HTML><HEAD>");
         Put_String(TFile,"<META HTTP-EQUIV=" & '"' &  "Content-Type" & '"' & " content=" & '"' & "text/html; charset=iso-8859-1" & '"' & '>');
         Put_String(TFile,"<TITLE>Manual</TITLE></HEAD>");
      end if;
      Put_String(TFile,"<pre>",false);
      while not isEOF(SFile) loop
         Byte := Get_Byte(SFile);
         if Byte in 1..255 then
            case Character'Val(Byte) is
            when Copyright_Sign              => Put_String(TFile,"&copy;",false);
            when Registered_Trade_Mark_Sign  => Put_String(TFile,"&reg;",false);
            when Inverted_Exclamation        => Put_String(TFile,"&iexcl;",false);
            when Pound_Sign                  => Put_String(TFile,"&pound;",false);
            when Yen_Sign                    => Put_String(TFile,"&yen;",false);
            when Broken_Bar                  => Put_String(TFile,"&brvbar;",false);
            when Section_Sign                => Put_String(TFile,"&sect;",false);
            when Left_Angle_Quotation        => Put_String(TFile,"&laquo;",false);
            when Right_Angle_Quotation       => Put_String(TFile,"&raquo;",false);
            when Not_Sign                    => Put_String(TFile,"&not;",false);
            when Degree_Sign                 => Put_String(TFile,"&deg;",false);
            when Plus_Minus_Sign             => Put_String(TFile,"&plusmn;",false);
            when Superscript_One             => Put_String(TFile,"&sup1;",false);
            when Superscript_Two             => Put_String(TFile,"&sup2;",false);
            when Superscript_Three           => Put_String(TFile,"&sup3;",false);
            when Micro_Sign                  => Put_String(TFile,"&micro;",false);
            when Middle_Dot                  => Put_String(TFile,"&middot;",false);
            when Fraction_One_Quarter        => Put_String(TFile,"&frac14;",false);
            when Fraction_One_Half           => Put_String(TFile,"&frac12;",false);
            when Inverted_Question           => Put_String(TFile,"&iquest;",false);
            when Fraction_Three_Quarters     => Put_String(TFile,"&frac34;",false);
            when UC_AE_Diphthong             => Put_String(TFile,"&AElig;",false);
            when UC_A_Acute                  => Put_String(TFile,"&Aacute;",false);
            when UC_A_Circumflex             => Put_String(TFile,"&Acirc;",false);
            when UC_A_Grave                  => Put_String(TFile,"&Agrave;",false);
            when UC_A_Ring                   => Put_String(TFile,"&Aring;",false);
            when UC_A_Tilde                  => Put_String(TFile,"&Atilde;",false);
            when UC_A_Diaeresis              => Put_String(TFile,"&Auml;",false);
            when UC_C_Cedilla                => Put_String(TFile,"&Ccedil;",false);
            when UC_Icelandic_Eth            => Put_String(TFile,"&ETH;",false);
            when UC_E_Acute                  => Put_String(TFile,"&Eacute;",false);
            when UC_E_Circumflex             => Put_String(TFile,"&Ecirc;",false);
            when UC_E_Grave                  => Put_String(TFile,"&Egrave;",false);
            when UC_E_Diaeresis              => Put_String(TFile,"&Euml;",false);
            when UC_I_Acute                  => Put_String(TFile,"&IAcute;",false);
            when UC_I_Circumflex             => Put_String(TFile,"&Icirc;",false);
            when UC_I_Grave                  => Put_String(TFile,"&Igrave;",false);
            when UC_I_Diaeresis              => Put_String(TFile,"&Iuml;",false);
            when UC_N_Tilde                  => Put_String(TFile,"&Ntilde;",false);
            when UC_O_Acute                  => Put_String(TFile,"&Oacute;",false);
            when UC_O_Circumflex             => Put_String(TFile,"&Ocirc;",false);
            when UC_O_Grave                  => Put_String(TFile,"&Ograve;",false);
            when UC_O_Oblique_Stroke         => Put_String(TFile,"&Oslash;",false);
            when UC_O_Tilde                  => Put_String(TFile,"&Otilde;",false);
            when UC_O_Diaeresis              => Put_String(TFile,"&ouml;",false);
            when UC_Icelandic_Thorn          => Put_String(TFile,"&thorn;",false);
            when UC_U_Acute                  => Put_String(TFile,"&Uacute;",false);
            when UC_U_Circumflex             => Put_String(TFile,"&Ucirc;",false);
            when UC_U_Grave                  => Put_String(TFile,"&Ugrave;",false);
            when UC_U_Diaeresis              => Put_String(TFile,"&Uuml;",false);
            when UC_Y_Acute                  => Put_String(TFile,"&Yacute;",false);
            when LC_AE_Diphthong             => Put_String(TFile,"&aelig;",false);
            when LC_A_Acute                  => Put_String(TFile,"&aacute;",false);
            when LC_A_Circumflex             => Put_String(TFile,"&acirc;",false);
            when LC_A_Grave                  => Put_String(TFile,"&agrave;",false);
            when LC_A_Ring                   => Put_String(TFile,"&aring;",false);
            when LC_A_Tilde                  => Put_String(TFile,"&atilde;",false);
            when LC_A_Diaeresis              => Put_String(TFile,"&auml;",false);
            when LC_C_Cedilla                => Put_String(TFile,"&ccedil;",false);
            when LC_Icelandic_Eth            => Put_String(TFile,"&ETH;",false);
            when LC_E_Acute                  => Put_String(TFile,"&eacute;",false);
            when LC_E_Circumflex             => Put_String(TFile,"&ecirc;",false);
            when LC_E_Grave                  => Put_String(TFile,"&egrave;",false);
            when LC_E_Diaeresis              => Put_String(TFile,"&euml;",false);
            when LC_I_Acute                  => Put_String(TFile,"&iacute;",false);
            when LC_I_Circumflex             => Put_String(TFile,"&icirc;",false);
            when LC_I_Grave                  => Put_String(TFile,"&igrave;",false);
            when LC_I_Diaeresis              => Put_String(TFile,"&iuml;",false);
            when LC_N_Tilde                  => Put_String(TFile,"&ntilde;",false);
            when LC_O_Acute                  => Put_String(TFile,"&oacute;",false);
            when LC_O_Circumflex             => Put_String(TFile,"&ocirc;",false);
            when LC_O_Grave                  => Put_String(TFile,"&ograve;",false);
            when LC_O_Oblique_Stroke         => Put_String(TFile,"&oslash;",false);
            when LC_O_Tilde                  => Put_String(TFile,"&otilde;",false);
            when LC_O_Diaeresis              => Put_String(TFile,"&ouml;",false);
            when LC_Icelandic_Thorn          => Put_String(TFile,"&thorn;",false);
            when LC_U_Acute                  => Put_String(TFile,"&uacute;",false);
            when LC_U_Circumflex             => Put_String(TFile,"&ucirc;",false);
            when LC_U_Grave                  => Put_String(TFile,"&ugrave;",false);
            when LC_U_Diaeresis              => Put_String(TFile,"&uuml;",false);
            when LC_Y_Acute                  => Put_String(TFile,"&yacute;",false);
            when LC_Y_Diaeresis              => Put_String(TFile,"&yuml;",false);
--            when ' '                         => Put_String(TFile,"&nbsp;",false);
--            when HT                          => Put_String(TFile,"&nbsp; &nbsp;",false);
            when '<'                         => Put_String(TFile,"&lt;",false);
            when '>'                         => Put_String(TFile,"&gt;",false);
            when '&'                         => Put_String(TFile,"&amp;",false);
            when Quotation                   => Put_String(TFile,"&quot;",false);
--            when LF|CR                       =>
--                        
--               if Previous >-1 and then Character'Val(Previous) = '.' then
--                  Put_String(TFile,"<BR>");
--               elsif (Previous = 10 or Previous = 13) then
--                  Put_String(TFile,"<P>");
--               else
--                  Put_String(TFile,"<BR>");
--               end if;
            when others => Put_Byte(TFile,Byte);
            end case;
--            Previous := Byte;
         end if;
      end loop;
      Put_String(TFile,"</pre>",false);
--      Put_String(TFile,VString,true);
      -- Footer
      if Argument_Count = 5 then
         declare
            Footer : FileHandle_Type(new UString'(U(Argument(5))),Read);
         begin
            while not isEOF(Footer) loop
               Byte := Get_Byte(Footer);
               Put_Byte(TFile,Byte);
            end loop;
         end;
      end if;
      Put_String(TFile,"</BODY></HTML>");
      Set_File_Type(Target,16#FAF#);
   end Main;
begin
   if ((Argument_Count < 2) or (Argument_Count > 5)) then
      Ada.Text_IO.Put_Line ("**** Wrong number of arguments (<2|>4) ****");
      Ada.Text_IO.Put_Line ("Use : Text2HTML <source> <target> [<Header>] [<BODY-TAG>] [<Footer>]");
      Ada.Text_IO.New_Line;
   else
      Main;
   end if;
end Text2HTML;
