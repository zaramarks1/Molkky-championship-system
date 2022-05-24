package com.molkky.molkky.service;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.utility.StringUtilities;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

@SpringBootTest
class SearchServiceTest {
    @Autowired
    private SearchService searchService;
    @Autowired
    private TournamentRepository tournamentRepository;
    @Test
    void searchPreciseTournament() throws InterruptedException {
//        given
        Tournament tournament = new Tournament();
        String randomName = StringUtilities.createCode(20);
        tournament.setName(randomName);
        tournamentRepository.save(tournament);
//        when
        List<TournamentModel> found = searchService.searchTournaments(randomName);
//        then
        Assertions.assertEquals(1, found.size());
        Assertions.assertEquals(randomName, found.get(0).getName());
    }
    @Test
    void searchLessOneLetterTournament() throws InterruptedException {
//        given
        Tournament tournament = new Tournament();
        String randomName = StringUtilities.createCode(20);
        StringBuilder sb = new StringBuilder(randomName);
        sb.deleteCharAt(19);
        String randomName2 = sb.toString();

        tournament.setName(randomName);
        tournamentRepository.save(tournament);
//        when
        List<TournamentModel> found = searchService.searchTournaments(randomName2);
//        then
        Assertions.assertEquals(1, found.size());
        Assertions.assertEquals(randomName, found.get(0).getName());
    }
}
