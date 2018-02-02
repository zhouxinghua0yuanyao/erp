REM dbdrv: sql ~PROD ~PATH ~FILE none none none package &phase=pls \
REM dbdrv: checkfile(120.0.12010000.4=120.1.12020000.3)(115.2=120.1):~PROD:~PATH:~FILE     

create or replace package GME_BATCH_ACTION_OPEN_INTF AS
/* $Header: GMEBAOIS.pls 120.1.12020000.3 2013/04/26 07:18:35 shalchen noship $ */

  -- Author  : CHENSL
  -- Created : 2013/1/24 18:37:03
  -- Purpose : This package is used to process batch action import from OPM batch action open interface
  
  -- Public type declarations
  TYPE gme_batch_action_intf_type IS TABLE OF GME_BATCH_ACTIONS_INTERFACE%ROWTYPE INDEX BY BINARY_INTEGER;

  gme_batch_action_intf_tbl gme_batch_action_intf_type;
  
  TYPE gme_dist_batch_action_rec IS RECORD (int_organization_id gme_batch_actions_interface.int_organization_id%TYPE,
                                            int_batch_id        gme_batch_actions_interface.int_batch_id%TYPE,
                                            object_type         gme_batch_actions_interface.object_type%TYPE,
                                            action              gme_batch_actions_interface.action%TYPE,
                                            action_seq          NUMBER);                                            
                                            
  TYPE gme_dist_batch_action_type IS TABLE OF gme_dist_batch_action_rec INDEX BY BINARY_INTEGER;
  
  gme_dist_batch_action_tbl gme_dist_batch_action_type;
                                              
  
  
  -- Public constant declarations


  -- Public variable declarations

  -- Public function and procedure declarations
PROCEDURE process_batch_actions     (err_buf                   OUT NOCOPY VARCHAR2,
                                     ret_code                  OUT NOCOPY VARCHAR2,
                                     p_purge_flag              IN VARCHAR2 DEFAULT 'Y', 
                                     p_max_no_of_worker        IN NUMBER DEFAULT 5,
                                     p_max_rows_of_per_worker  IN NUMBER DEFAULT 1000);  
                                     
PROCEDURE derive_int_id_columns(p_request_id            IN             NUMBER
                               ,x_return_message       OUT NOCOPY      VARCHAR2
                               ,x_return_status        OUT NOCOPY      VARCHAR2);
                          
PROCEDURE batch_actions_bulk_validation( p_group_id              IN NUMBER
                                        ,p_request_id            IN NUMBER
                                        ,x_return_message       OUT NOCOPY      VARCHAR2
                                        ,x_return_status        OUT NOCOPY      VARCHAR2);                
                          
                          
                      

PROCEDURE launch_action_worker(err_buf          OUT NOCOPY VARCHAR2,
                               ret_code         OUT NOCOPY VARCHAR2,
                               p_group_id       IN NUMBER,
                               p_purge_flag     IN VARCHAR2);
                               
FUNCTION  gme_log_interface_err (P_GROUP_ID             NUMBER,
                                P_object_type          VARCHAR2,
                                P_INTERFACE_ID         NUMBER,
                                P_COLUMN_NAME          VARCHAR2 DEFAULT NULL,
                                P_MESSAGE_TYPE         VARCHAR2 DEFAULT 'E',
                                P_MESSAGE_NAME         VARCHAR2,
                                P_MESSAGE_TEXT         VARCHAR2 DEFAULT NULL,
                                P_APPLICATION          VARCHAR2 DEFAULT 'GME',
                                P_SOURCE_TABLE_NAME    VARCHAR2,
                                P_ERR_MESSAGE_DETAIL   GME_BATCH_OPEN_INTERFACE.T_MESSAGE_TABLE,
                                P_PARAM1               VARCHAR2 DEFAULT NULL,
                                P_PARAM2               VARCHAR2 DEFAULT NULL,
                                P_PARAM3               VARCHAR2 DEFAULT NULL,
                                P_PARAM4               VARCHAR2 DEFAULT NULL,
                                P_PARAM5               VARCHAR2 DEFAULT NULL,
                                P_PARAM6               VARCHAR2 DEFAULT NULL,
                                P_TOKEN1               VARCHAR2 DEFAULT NULL,
                                P_TOKEN2               VARCHAR2 DEFAULT NULL,
                                P_TOKEN3               VARCHAR2 DEFAULT NULL,
                                P_TOKEN4               VARCHAR2 DEFAULT NULL,
                                P_TOKEN5               VARCHAR2 DEFAULT NULL,
                                P_TOKEN6               VARCHAR2 DEFAULT NULL)   
     RETURN INTEGER;
   
   
PROCEDURE validation_error_process   (p_request_id       IN  NUMBER
                                     ,p_group_id         IN  NUMBER
                                     ,p_application      IN  VARCHAR2 DEFAULT 'GME'
                                     ,p_message_name	   IN  VARCHAR2
                                     ,p_column_name      IN  VARCHAR2 DEFAULT NULL
                                     ,p_message_type     IN  VARCHAR2 DEFAULT 'E'
                                     ,p_message_text     IN  VARCHAR2 DEFAULT NULL
                                     ,p_param1           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param2           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param3           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param4           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param5           IN  VARCHAR2 DEFAULT NULL
                                     ,p_param6           IN  VARCHAR2 DEFAULT NULL                                    
                                     ,x_return_message       OUT NOCOPY      VARCHAR2
                                     ,x_return_status        OUT NOCOPY      VARCHAR2);
                                     
PROCEDURE check_unprocessed_resc_trxn(p_object_type           IN NUMBER
                                         ,p_int_batch_id          IN NUMBER
                                         ,p_int_batchstep_id      IN NUMBER
                                         ,x_return_message       OUT NOCOPY      VARCHAR2
                                         ,x_return_status        OUT NOCOPY      VARCHAR2);                             
   
end GME_BATCH_ACTION_OPEN_INTF;

/       
COMMIT ;
EXIT;   
