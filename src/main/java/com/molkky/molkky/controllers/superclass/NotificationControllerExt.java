package com.molkky.molkky.controllers.superclass;

import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;

@Controller
public class NotificationControllerExt {
    @Autowired
    private NotificationService notificationService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @ModelAttribute("unreadCount")
    public int getUnreadCount(Model model) {
        UserLogged userLogged = (UserLogged) model.getAttribute("user");
        if(userLogged == null) return 0;
        return notificationService.getUnreadNotificationCount(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
    }
}
