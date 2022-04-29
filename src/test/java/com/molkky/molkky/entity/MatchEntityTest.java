package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.SetRepository;
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
    private SetRepository setRepository;
    @Autowired
    private TeamRepository teamRepository;

    @Test
    void testCreateMatch(){
//        given
        Match match = new Match();
        match.setNbSets(3);
//        when

//        then
        Assertions.assertFalse(match.getFinished());
        Assertions.assertEquals(3, match.getNbSets());
    }

    @Test
    void testInsertMatch(){
//        given
        Match match = new Match();
        match.setNbSets(3);
        Match nvMatch = matchRepository.save(match);
//        when

//        then
        Assertions.assertEquals(match.getFinished(), nvMatch.getFinished());
        Assertions.assertEquals(match.getNbSets(), nvMatch.getNbSets());
    }

    @Test
    void testInsertMatchWithSets(){
//        given
        Match match = new Match();
        match.setNbSets(3);
        match.setSets(createSets(match.getNbSets()));
        Match nvMatch = matchRepository.save(match);
//        when

//        then
        Assertions.assertEquals(match.getFinished(), nvMatch.getFinished());
        Assertions.assertEquals(match.getNbSets(), nvMatch.getNbSets());
        Assertions.assertEquals(match.getNbSets(), nvMatch.getSets().size());
//        verify that the sets are in DB
        for (int i = 0; i < match.getNbSets(); i++){
            Assertions.assertEquals(match.getSets().get(i).getId(), setRepository.findById(match.getSets().get(i).getId()).getId());
        }
    }

    @Test
    void testInsertMatchWithTeams(){
//        given
        Match match = new Match();
        match.setNbSets(3);
        List<Team> teams = createTeams(4);
        match.setTeams(teams);
        match.setWinner(teams.get(0));
        Match nvMatch = matchRepository.save(match);
//        when

//        then
        Assertions.assertEquals(teams.get(0).getId(), match.getWinner().getId());
        Assertions.assertEquals(4, match.getTeams().size());

//        verify that the sets are in DB
    }

    List<Set> createSets(Integer n){
        List<Set> sets = new ArrayList<>();
        for(int i = 0; i < n; i++){
            Set set = new Set();
            sets.add(set);
        }
        return sets;
    }

    List<Team> createTeams(Integer n){
        List<Team> teams = new ArrayList<>();
        for(int i = 0; i < n; i++){
            Team team = new Team();
            teams.add(teamRepository.save(team));
        }
        return teams;
    }
}
