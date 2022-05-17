package com.molkky.molkky.service;


import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

import java.util.List;

@SpringBootTest
class UserTournamentRoleServiceTest {
    @Autowired
    private UserTournamentRoleService userTournamentRoleService;
    @Autowired
    private TournamentRepository tournamentRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Test
    void getUserTournamentRoleFromModelTest() {
        UserTournamentRole userTournamentRole = userTournamentRoleRepository.save(new UserTournamentRole());
        UserTournamentRoleModel userTournamentRoleModel = new UserTournamentRoleModel();
        userTournamentRoleModel.setId(userTournamentRole.getId());
        Assertions.assertEquals(userTournamentRole.getId(), userTournamentRoleService.getUserTournamentRoleFromModel(userTournamentRoleModel).getId());
    }

     @Test
    void getTournamentStaffFromUserTest(){
//        given
         UserTournamentRole user = userTournamentRoleRepository.save(new UserTournamentRole());
         UserTournamentRole user2 = userTournamentRoleRepository.save(new UserTournamentRole());
         Tournament tournament = tournamentRepository.save(new Tournament());
         user.setTournament(tournament);
         user.setRole(UserRole.STAFF);
         user2.setTournament(tournament);
         userTournamentRoleRepository.save(user);
         userTournamentRoleRepository.save(user2);
//         when
         List<UserTournamentRole> users = userTournamentRoleService.getTournamentStaffFromUser(new UserTournamentRoleModel(user2));
//         then
         Assertions.assertEquals(user.getId(), users.get(0).getId());
     }
}
