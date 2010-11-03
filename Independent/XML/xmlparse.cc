/*
(C) 2006 Andy Adler. This is free software; you can redistribute and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2, or (at
    your option) any later version.
    */

#include <expat.h>
#include <octave/oct.h>

std::string chardata;

void indent( int depth, char ch)
{
   putchar('\n');
   for( int i=0; i< depth; i++) putchar(ch);
}

void 
startElement(void *userData, const char *name, const char **atts)
{
    int *depthPtr = (int *)userData;

    indent( *depthPtr, '=');
    printf(name);

    for (int i = 0; atts[i]; i += 2) {
        indent( *depthPtr, ' ');
        printf(" .ATTRIBUTE %s='%s'", atts[i], atts[i + 1]);
    }
    *depthPtr += 1;
    chardata= "";

}

void 
endElement(void *userData, const char *name)
{
    int *depthPtr = (int *)userData;
    indent( *depthPtr, ' ');
    printf(" .CHARDATA=(%s)", chardata.c_str() );
    *depthPtr -= 1;
}

void
charData(void *userData, const char *s, int len)
{
    chardata.append( s, len);
}


DEFUN_DLD (xmlparse, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} @var{struct} = xmlparse (@var{str}, @var{xpath})\n\
Read a XML \n\
Based on the libexpat XML parser\n\
\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;
  if (nargin < 1)
     return retval;
  if (!args(0).is_string())
     return retval;

  // Open XML file
  const char * fname= args(0).string_value().c_str();
  FILE *fid = fopen( fname, "rb");
  if (!fid) {
     error("Can't open file (%s)", fname );
     return retval;
  }

  // Create XML Parser
  XML_Parser parser = XML_ParserCreate(NULL);
  if (! parser) {
     error("Couldn't allocate memory for XML parser\n");
     return retval;
  }


  char buf[1024];
  int depth = 0, done=0;
  XML_SetUserData(parser, &depth);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, charData);
  while (!done) {
     size_t len = fread(buf, 1, sizeof(buf), fid);
     done = len < sizeof(buf);
     if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR) {
        error("XML_Parser error %s at line %d\n",
              XML_ErrorString(XML_GetErrorCode(parser)),
              XML_GetCurrentLineNumber(parser));
        XML_ParserFree(parser);
        return retval;
     }
  }

  XML_ParserFree( parser );

  return retval;
}
