package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Court;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.CourtRepository;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.repository.PoolRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.ArrayList;
import java.util.List;

@SpringBootTest(classes = MolkkyApplication.class)
class SetEntityTest {
    @Autowired
    private SetRepository setRepository;
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


        Set set = setRepository.save(new Set(court, teams));
//        match.setTeams(teams);
//        match.setCourt(court);
//        matchRepository.save(match);


        Assertions.assertEquals("court_testMatch", set.getCourt().getName(), "Court name is not correct");
        Assertions.assertEquals(2, set.getTeams().size(), "Number of teams is not correct");

        Set recupSet = setRepository.findById(set.getId());
        Assertions.assertNotNull(recupSet, "Match not found");
    }
}
