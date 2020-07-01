/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
import kroneckerbio.parser.RuleBasedModelParser._
import kroneckerbio.LoadRuleBasedKroneckerModel

object Test {
  def main(args:Array[String]) {

    println("Hello, David!")

    def printParsed(parsed: ParseResult[Any]) =
      parsed match {
        case Success(output,_) => println(output)
        case x => println(x)
      }

    //printParsed(parseAll(compartment, "cell_membrane 2 12.3"))
    //printParsed(parseAll(agent, "egfr(l,d,Y1086~P)"))
    //printParsed(parseAll(seed, "egf(l-1).egfr(d,l-1,Y1086~P) 1"))
    //printParsed(parseAll(observable, "EGFR#pY1086 egfr(Y1086~P).{(egf|amphiregulin)&tgfaplha}"))
    //printParsed(parseAll(parameter, "koff 100"))
    //printParsed(parseAll(rule, "egf(l) + egfr(l,d) <-> egf(l-1).egfr(l-1,d) kfwd krev EGFonEGFR"))
    //printParsed(parseAll(compartment_section, "% Compartments\r\nextracellular 3 100\r\ncell_membrane 2 12.3\r\ncell          3 1.2"))
    //printParsed(parseAll(model_sections, "% Compartments\r\nextracellular 3 100\r\ncell_membrane 2 12.3\r\ncell          3 1.2"))
    //printParsed(parseAll(observable_section, "% Observables"))
    //val m = LoadRuleBasedKroneckerModel(List("src/test/ConsumptionPartial2AsymRuleAsymFrag.txt"))
    //val m = LoadRuleBasedKroneckerModel(List("src/test/ActivatedDimerization.txt"))
    //val m = LoadRuleBasedKroneckerModel(List("src/test/DimensionalRestriction2.txt"))
    //val m = LoadRuleBasedKroneckerModel(List("src/test/DimerPhosphorylation.txt"))
    //val m = LoadRuleBasedKroneckerModel(List("src/test/MonomerConsumption.txt"))
    //val m = LoadRuleBasedKroneckerModel(List("src/test/MonomerConsumption2.txt"))
    //val m = LoadRuleBasedKroneckerModel(List("src/test/ErbB1Adaptors.txt"))
    val m = LoadRuleBasedKroneckerModel(List("src/test/ErbBModel_10451173.txt"))
    m.Validate()
    m.Finalize()
    m.ExportToMatlab("src/test/KroneckerModel.txt")
  }
}

