package com.molkky.molkky.service;

import com.molkky.molkky.domain.Club;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest
class ClubServiceTest {

    @MockBean
    private ClubService clubService;

    @Test
    void getClubsByName(){
        List<Club> clubs = new ArrayList<>();
        Mockito.when(clubService.getClubsByName("test")).thenReturn(clubs);

        Assertions.assertEquals(clubs, clubService.getClubsByName("test"));
        Mockito.verify(clubService, Mockito.times(1)).getClubsByName("test");
    }
}
