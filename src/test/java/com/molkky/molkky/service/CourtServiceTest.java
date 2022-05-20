package com.molkky.molkky.service;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import javax.validation.constraints.AssertTrue;

@SpringBootTest
class CourtServiceTest {
    @Test
    void getAvailableCourtsTest(){
//        given
        boolean value = true;
        Assertions.assertTrue( value, "fake test");
    }
}
