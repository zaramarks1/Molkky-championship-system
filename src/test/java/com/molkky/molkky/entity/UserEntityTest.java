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
import type.UserRole;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.List;

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
                true,
                UserRole.PLAYER

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

        User user1 = new User("pseudoUser1", "surname1", "forename1", "club1", "email1", false, UserRole.PLAYER);
        List<User> users = new ArrayList<>();
        users.add(user1);
        team.setUsers(users);
//        user1.setTeam(team);
        userRepository.save(user1);
        Assertions.assertEquals(1, team.getUsers().size(), "Team size is not correct");
        User recupUser = userRepository.findById(user1.getId());
        //Assertions.assertEquals(1, recupUser.getTeams().size(), "Team size is not correct");
    }
}
