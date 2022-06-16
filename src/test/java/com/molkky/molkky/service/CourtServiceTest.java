package com.molkky.molkky.service;

import com.molkky.molkky.model.CourtModel;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

@SpringBootTest
class CourtServiceTest {

    @Autowired
    private CourtService courtService;

    @MockBean
    private CourtModel courtModel;

    @Test
    void testFunctions(){
//        given
        courtService.getAvailableCourts();
        courtService.getCourtFromModel(courtModel);

    }
}
