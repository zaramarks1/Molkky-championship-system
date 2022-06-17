package com.molkky.molkky.service;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.repository.CourtRepository;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.List;

@SpringBootTest
class CourtServiceTest {

    @Autowired
    private CourtService courtService;

    @MockBean
    private CourtModel courtModel;

    @MockBean
    private CourtRepository courtRepository;

    @Test
    void testFunctions(){
//        given
        List<Court> f1 = courtService.getAvailableCourts();
        Court f2 = courtService.getCourtFromModel(courtModel);

        Mockito.verify(courtRepository, Mockito.times(1)).findByAvailable(true);
        Mockito.verify(courtRepository, Mockito.times(1)).findById(Mockito.anyInt());

        Mockito.verifyNoMoreInteractions(courtRepository);
    }
}
