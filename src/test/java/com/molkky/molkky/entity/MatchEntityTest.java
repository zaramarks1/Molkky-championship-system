package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Court;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.CourtRepository;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.PoolRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class MatchEntityTest {
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private CourtRepository courtRepository;
    @Autowired
    private PoolRepository poolRepository;

    @Test
    void testInsertMatch() {
//        création des équipes du match
//        Team team1 = new Team("team1_testMatch", 2);
//        Team team2 = new Team("team2_testMatch", 2);
        Team team1 = teamRepository.save(new Team("team1_testMatch", 2));
        Team team2 = teamRepository.save(new Team("team2_testMatch", 2));
        List<Team> teams = new ArrayList<>();
        teams.add(team1);
        teams.add(team2);
        Court court = courtRepository.save(new Court(true, "court_testMatch"));
//        Court court = new Court(true, "court_testMatch");


        Match match = matchRepository.save(new Match(court, teams));
//        match.setTeams(teams);
//        match.setCourt(court);
//        matchRepository.save(match);


        Assertions.assertEquals("court_testMatch", match.getCourt().getName(), "Court name is not correct");
        Assertions.assertEquals(2, match.getTeams().size(), "Number of teams is not correct");

        Match recupMatch = matchRepository.findById(match.getId());
        Assertions.assertNotNull(recupMatch, "Match not found");
    }
}
