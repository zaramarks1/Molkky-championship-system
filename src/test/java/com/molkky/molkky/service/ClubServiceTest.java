package com.molkky.molkky.service;

import com.molkky.molkky.domain.Club;
import com.molkky.molkky.repository.ClubRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest
class ClubServiceTest {

    @Autowired
    private ClubService clubService;
    @MockBean
    private ClubRepository clubRepository;

    @Test
    void getClubsByName(){
        List<Club> clubs = new ArrayList<>();
        Club club = new Club();
        club.setName("Test");
        clubs.add(club);
        clubRepository.save(club);

        List<Club> result = clubService.getClubsByName("Test");

        Assertions.assertEquals(clubs, result);
    }
}
