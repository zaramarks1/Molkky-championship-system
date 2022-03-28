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
class UserEntityTest {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private TeamRepository teamRepository;

    @Test
    void testInsertUser() {
        User user = userRepository.save(new User(
                "pseudo_test",
                "surname_test",
                "forename_test",
                "club_test",
                "email_test",
                true
        ));
        Assertions.assertEquals("pseudo_test", user.getPseudo(), "Pseudo is not correct");
        User recupUser = userRepository.findById(user.getId());
        Assertions.assertEquals("pseudo_test", recupUser.getPseudo(), "Pseudo is not correct");
    }

    @Test
    @Rollback(false)
    @Transactional
    void testUserToTeam(){
        Team team = teamRepository.save(new Team("team_test", 2));

        User user1 = new User("pseudoUser1", "surname1", "forename1", "club1", "email1", false);
        Set<Team> teams = new HashSet<>();
        teams.add(team);
        user1.setTeams(teams);
        userRepository.save(user1);
        Assertions.assertEquals(1, user1.getTeams().size(), "Team size is not correct");
        User recupUser = userRepository.findById(user1.getId());
        Assertions.assertEquals(1, recupUser.getTeams().size(), "Team size is not correct");
    }
}
