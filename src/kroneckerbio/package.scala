/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
import kroneckerbio.model._
import kroneckerbio.parser.RuleBasedModelParser.{parseAll,model_sections,Success}
import kroneckerbio.parser._
import java.io.FileReader

package object kroneckerbio {

  implicit def implicitOption[A](a: A): Option[A] = Some(a)
  implicit def traversableToSeq[A](a: Traversable[A]) = a.toSeq

  def LoadRuleBasedKroneckerModel(filenames: Iterable[String]) = {
    // Read file and concatenate all sections found
    val parsed_sections = (for (filename <- filenames) yield
      parseAll(model_sections, new FileReader(filename)) match {
        case Success(result,_) => result
        case x                 => println(x); Nil
      }).flatten

    // Initialize model
    val m = new RuleBasedKroneckerModel()
    var name_found = false

    // Loop over sections and concatenate them into their respective
    for (section <- parsed_sections) {
      section match {
        case section: CompartmentSection =>
          // If the model does not have a name yet, and this compartment section provides one, copy the name
          //#todo Decide if I actually want to have new model names replace old ones
          if (section.name != None && name_found != false) {
            m.Name = section.name.get
            name_found = true}

          // Concatenate the compartments
          m.Compartments ++ section.lines

        case section: AgentSection =>
          // Concatenate all the agents
          m.Agents ++ section.lines

        case section: SeedSection =>
          // Extract the default compartment
          val default_compartment = section.compartment

          if (section.compartment != None) {
            // Loop over every seed and add it
            for (seed <- section.lines) {
              // Copy the old seed onto the new seed by mapping the set of monomers onto a new set of monomers,
              // setting the compartment to the default if it is None, then copying the seed value
              m.Seeds += Seed(
                SeedGraph(
                  seed.Graph.Monomers.map(old =>
                    SeedMonomer(
                      old.Name,
                      old.Sites,
                      old.Compartment.getOrElse(default_compartment.get)
                    )
                  )
                ),
                seed.Value
              )
            }
          } else {
            assert(
              section.lines.map(_.Graph).flatMap(_.Monomers).forall(_.Compartment != None),
              "A seed section has no compartment defined, but one of the member seeds has an unspecified compartment"
            )

            // Add each seed
            for (seed <- section.lines) {
              m.Seeds += Seed(
                SeedGraph(
                  seed.Graph.Monomers.map(old =>
                    SeedMonomer(
                      old.Name,
                      old.Sites,
                      old.Compartment.get
                    )
                  )
                ),
                seed.Value
              )
            }
          }

        case section: ObservableSection =>
          // If something is provided for the observable section compartment,
          // make the individual compartments equal to the section compartment
          if (section.compartment != None) {
            // Verify that all individual observable compartments are empty
            assert(section.lines.flatMap(
              _.PatternsOnly.map(_.Pattern.Compartment == AnyCompartmentSite)
              ).foldLeft(true)(_ && _),
              "A compartment was provided for an observable section and for one of the member observables.")

            // Concatenate the new observables
            m.Observables ++ section.lines.map(oldObs =>
              // Remake observables
              new Observable(
                // Copy the name
                oldObs.Name,

                // Remake the contributors
                oldObs.Contributors.map(
                  _ match {
                    // Don't remake the constants because they don't have compartments
                    case oldCon: ConstantContributor => oldCon

                    // Remake the patterns with the updated compartment
                    case oldCon: PatternContributor =>
                      oldCon.copy(
                        Pattern = oldCon.Pattern.copy(
                          Compartment = section.compartment.get
                        )
                      )
                  }
                )
              )
            )
          } else {
            // Just concatenate the observables without modifications
            m.Observables ++ section.lines
          }

        case section: ParameterSection =>
          m.Parameters ++ section.lines

        case section: RuleSection =>
          m.Rules ++ section.lines
      }
    }

    // Return
    m
  }
}
