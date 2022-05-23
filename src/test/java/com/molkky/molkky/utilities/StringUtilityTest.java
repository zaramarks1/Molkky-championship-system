package com.molkky.molkky.utilities;

import com.molkky.molkky.utility.StringUtilities;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class StringUtilityTest {
    @Test
    void testCodeLength(){
        for(int i = 0; i<20; i++){
            String code = StringUtilities.createCode(i);
            Assertions.assertEquals(code.length(),i);
        }
    }

    @Test
    void testCodeAlphanumeric(){
        String code = StringUtilities.createCode(500);
        Assertions.assertTrue(code.matches("^[a-zA-Z\\d]*$"));
    }
}
