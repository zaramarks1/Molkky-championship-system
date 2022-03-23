package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.repository.TeamRepository;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestComponent;

@SpringBootTest(classes = MolkkyApplication.class)
class TeamEntityTest {
    @Autowired
    private TeamRepository teamRepository;

    @Test
    void testInsertTeam() {
        Team team = teamRepository.save(new Team("Team 1", 2));
        Assertions.assertEquals("Team 1", team.getName(), "Team name is not correct");
        Team recupTeam = teamRepository.findById(team.getId());
        Assertions.assertEquals("Team 1", recupTeam.getName(), "Team name is not correct");
    }
}
