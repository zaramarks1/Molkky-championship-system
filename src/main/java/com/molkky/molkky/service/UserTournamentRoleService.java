package com.molkky.molkky.service;

import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.UserTournamentRoleModel;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.UserRole;

import java.util.List;

@Service
public class UserTournamentRoleService {
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    public UserTournamentRole getUserTournamentRoleFromModel(UserTournamentRoleModel userTournamentRoleModel){
        return userTournamentRoleRepository.findById(userTournamentRoleModel.getId());
    }

    public List<UserTournamentRole> getTournamentStaffFromUser(UserTournamentRoleModel userTournamentRoleModel){
        UserTournamentRole user = getUserTournamentRoleFromModel(userTournamentRoleModel);
        return userTournamentRoleRepository.findUserTournamentRoleByRoleAndAndTournament(UserRole.STAFF, user.getTournament());
    }
}
