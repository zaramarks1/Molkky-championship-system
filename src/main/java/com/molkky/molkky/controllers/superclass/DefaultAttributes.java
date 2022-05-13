package com.molkky.molkky.controllers.superclass;

import com.molkky.molkky.model.NotificationModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import com.molkky.molkky.service.NotificationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpSession;
import java.util.ArrayList;
import java.util.List;

@Controller
public class DefaultAttributes {
    private static final Logger logger = LoggerFactory.getLogger(DefaultAttributes.class);

    @Autowired
    private NotificationService notificationService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @ModelAttribute("user")
    public UserLogged getUser(HttpSession session) {
        UserLogged userLogged = session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
        if(userLogged != null) logger.info("getUser userlogged: ${}", userLogged);
        return userLogged;
    }

    @ModelAttribute("unreadCount")
    public Integer getUnreadCount(HttpSession session, Model model) {
        UserLogged userLogged = model.getAttribute("user") == null ? (UserLogged) session.getAttribute("user") : (UserLogged) model.getAttribute("user");
        if(userLogged != null) logger.info("getUnreadCount userlogged: {}" ,userLogged);

        if(userLogged == null) {
            logger.info("User not logged");
            return 0;
        }
        return notificationService.getUnreadNotificationCount(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
    }

    @ModelAttribute("notifications")
    public List<NotificationModel> getNotifications(HttpSession session) {
        UserLogged userLogged = session.getAttribute("user") != null ? (UserLogged) session.getAttribute("user") : null;
        if(userLogged == null) return new ArrayList<>();
        return notificationService.getNotificationModels(userTournamentRoleRepository.findById(userLogged.getTournamentRoleId()));
    }
}
