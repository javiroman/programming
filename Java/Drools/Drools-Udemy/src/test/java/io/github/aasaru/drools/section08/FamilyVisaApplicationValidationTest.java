/*
 *  Drools Online Course Sample Code and Study Materials (c) by Juhan Aasaru.
 *
 *  Drools Online Course Sample Code and Study Materials is licensed under a
 *  Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
 *
 *  You should have received a copy of the license along with this
 *  work. If not, see <http://creativecommons.org/licenses/by-nc-nd/4.0/>.
 */
package io.github.aasaru.drools.section08;

import org.junit.jupiter.api.Test;

public class FamilyVisaApplicationValidationTest {

  @Test
  public void shouldExecuteAllSteps() {
    FamilyVisaApplicationValidation.execute(1);
    FamilyVisaApplicationValidation.execute(2);
    FamilyVisaApplicationValidation.execute(3);
    FamilyVisaApplicationValidation.execute(4);
  }

}
