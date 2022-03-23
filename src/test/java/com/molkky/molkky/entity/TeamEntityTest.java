package com.molkky.molkky.entity;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.HashSet;
import java.util.Set;

@SpringBootTest(classes = MolkkyApplication.class)
class TeamEntityTest {
    @Autowired
    private TeamRepository teamRepository;
    @Autowired
    private UserRepository userRepository;

    @Test
    void testInsertTeam() {
        Team team = teamRepository.save(new Team("Team 1", 2));
        Assertions.assertEquals("Team 1", team.getName(), "Team name is not correct");
        Team recupTeam = teamRepository.findById(team.getId());
        Assertions.assertEquals("Team 1", recupTeam.getName(), "Team name is not correct");
    }

    @Test
    @Transactional
    @Rollback(false)
    void testTeamWithUsers(){
        Team team = teamRepository.save(new Team("Team_test", 2));
        Set<User> users = new HashSet<>();
        User user1 = userRepository.save(new User("pseudoUser1", "surname1", "forename1", "club1", "email1", false));
        User user2 = userRepository.save(new User("pseudoUser2", "surname2", "forename2", "club2", "email2", false));
        users.add(user1);
        users.add(user2);
        team.setUsers(users);
        teamRepository.save(team);
        Assertions.assertEquals(2, team.getUsers().size(), "Team has not 2 users");
        Team recupTeam = teamRepository.findById(team.getId());
        Set<User> recupUsers = recupTeam.getUsers();
        Assertions.assertEquals(2, recupUsers.size(), "Team has not 2 users");
        Assertions.assertEquals("pseudoUser1", recupUsers.iterator().next().getPseudo(), "User pseudo is not correct");
    }
}
