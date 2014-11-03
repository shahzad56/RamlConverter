RamlConverter: RAML -> WADL
=============

This converter is built to prepare RAMl API specification with payload described by Json Schema draft 3 for upload into Azure API Management

Features:

- Descriptions and json are converted into HTML embedded into WADL
- Limited support for markdown syntax in descriptions in schemas, json examples, and RAML itself
- Schemas and json examples parsed using Newtonsoft.Json 6.0.0
- Json examples are validated against schemas
- If you don't care about WADL and just want parser, AST can be built using:
        
    RamlParser.parseAst "schema.raml"



