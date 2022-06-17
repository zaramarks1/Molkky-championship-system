package com.molkky.molkky.service;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.repository.CourtRepository;
import com.molkky.molkky.repository.TournamentRepository;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@SpringBootTest
class CourtServiceTest {

    @Autowired
    private CourtService courtService;

    @MockBean
    private CourtModel courtModel;

    @MockBean
    private CourtRepository courtRepository;

    @MockBean
    private TournamentRepository tournamentRepository;

    @Test
    void testFunctions(){
//        given
        Tournament tournament = new Tournament();
        List<Court> courts = List.of(new Court(), new Court());
        when(courtRepository.findByTournamentAndAvailable(any(Tournament.class), anyBoolean())).thenReturn(courts);
        courtService.getAvailableCourts(tournament);
        courtService.getCourtFromModel(courtModel);

        Mockito.verify(courtRepository, Mockito.times(1)).findByTournamentAndAvailable(any(Tournament.class), anyBoolean());
        Mockito.verify(courtRepository, Mockito.times(1)).findById(Mockito.anyInt());
    }
}
