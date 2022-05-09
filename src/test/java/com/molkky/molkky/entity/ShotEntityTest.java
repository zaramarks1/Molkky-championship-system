package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Set;
import com.molkky.molkky.domain.Shot;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.SetRepository;
import com.molkky.molkky.repository.ShotRepository;
import com.molkky.molkky.repository.TeamRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class)
class ShotEntityTest {
    @Autowired
    private ShotRepository shotRepository;
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private MatchRepository matchRepository;
    @Autowired
    private SetRepository setRepository;

    @Test
    void testInsertShot() {
        Match match = matchRepository.save(new Match());
        Set set = setRepository.save(new Set());

        Team team = teamRepository.save(new Team());
        team.setName("Team 1");
        Shot shot = new Shot();
        shot.setTeam(team);
        shot.setSet(set);
        shot.setScore(1);
        shot = shotRepository.save(shot);
        Assertions.assertEquals(1, shot.getScore());
        Shot shotRecup = shotRepository.findById(shot.getId());
        Assertions.assertEquals(1, shotRecup.getScore());
    }
}
