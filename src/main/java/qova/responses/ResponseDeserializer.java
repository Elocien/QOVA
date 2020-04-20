// package qova.responses;

// import com.fasterxml.jackson.databind.JsonNode;
// import com.fasterxml.jackson.databind.ser.std.StdSerializer;

// public class ResponseDeserializer extends StdSerializer<Response>{
    
//     public ResponseDeserializer() {
//         this(null);
//     }
 
//     public ResponseDeserializer(Class<?> vc) {
//         super(vc);
//     }

//     @Override
//     public Response deserialize(JsonParser parser, DeserializationContext deserializer) {
//         Response response = new Response();
//         ObjectCodec codec = parser.getCodec();
//         JsonNode node = codec.readTree(parser);
         
//         // try catch block
//         JsonNode textResponseNode = node.get("response");
//         ResponseType response = textResponseNode.asText()
//         response.settextResponse(response);
//         return Response;
//     }
// }