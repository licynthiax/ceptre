val () = print ("Ceptre!\n")

val () = 
   case CommandLine.arguments () of
      [ fname, "--asterism", json_out ] => ignore (Top.asterism fname json_out)
    | [ fname ] => ignore (Top.runFirst fname)
    | _ => (print ("Usage: "^CommandLine.name ()^" CEPTREFILE.cep\n");
           print ("OR: "^CommandLine.name ()^" CEPTREFILE.cep --asterism JSONFILE.json\n"))
